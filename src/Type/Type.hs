{-# OPTIONS_GHC -Wall #-}
module Type.Type where

import Control.Monad (when)
import Control.Monad.State (StateT, liftIO)
import qualified Control.Monad.State as State
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.UnionFind.IO as UF

import qualified AST.Type as T
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Reporting.Region as R



-- CONCRETE TYPES


type Type =
    TermN Variable


type Variable =
    UF.Point Descriptor


type TypeConstraint =
    Constraint Type Variable


type TypeScheme =
    Scheme Type Variable



-- TYPE PRIMITIVES


data Term1 a
    = App1 a a
    | Fun1 a a
    | EmptyRecord1
    | Record1 (Map.Map String a) a


data TermN a
    = PlaceHolder String
    | AliasN Var.Canonical [(String, TermN a)] (TermN a)
    | VarN a
    | TermN (Term1 (TermN a))


record :: Map.Map String (TermN a) -> TermN a -> TermN a
record fs rec =
  TermN (Record1 fs rec)



-- DESCRIPTORS


data Descriptor = Descriptor
    { _content :: Content
    , _rank :: Int
    , _mark :: Int
    , _copy :: Maybe Variable
    }


data Content
    = Structure (Term1 Variable)
    | Atom Var.Canonical
    | Var Flex (Maybe Super) (Maybe String)
    | Alias Var.Canonical [(String,Variable)] Variable
    | Error


data Flex
    = Rigid
    | Flex


data Super
    = Number
    | Comparable
    | Appendable
    | CompAppend
    deriving (Eq)


noRank :: Int
noRank = -1


outermostRank :: Int
outermostRank = 0


noMark :: Int
noMark = 0


initialMark :: Int
initialMark = 1


occursMark :: Int
occursMark =
  -1


getVarNamesMark :: Int
getVarNamesMark =
  -2



-- CONSTRAINTS


data Constraint a b
    = CTrue
    | CSaveEnv
    | CEqual Error.Hint R.Region a a
    | CAnd [Constraint a b]
    | CLet [Scheme a b] (Constraint a b)
    | CInstance R.Region SchemeName a


type SchemeName = String


data Scheme a b = Scheme
    { _rigidQuantifiers :: [b]
    , _flexibleQuantifiers :: [b]
    , _constraint :: Constraint a b
    , _header :: Map.Map String (A.Located a)
    }



-- TYPE HELPERS


infixr 9 ==>


(==>) :: Type -> Type -> Type
(==>) a b =
  TermN (Fun1 a b)


(<|) :: TermN a -> TermN a -> TermN a
(<|) f a =
  TermN (App1 f a)



-- VARIABLE CREATION


mkDescriptor :: Content -> Descriptor
mkDescriptor content =
  Descriptor
    { _content = content
    , _rank = noRank
    , _mark = noMark
    , _copy = Nothing
    }


mkAtom :: Var.Canonical -> IO Variable
mkAtom name =
  UF.fresh $ mkDescriptor (Atom name)


mkVar :: Maybe Super -> IO Variable
mkVar maybeSuper =
  UF.fresh $ mkDescriptor (Var Flex maybeSuper Nothing)


mkNamedVar :: Flex -> String -> IO Variable
mkNamedVar flex name =
    UF.fresh $ mkDescriptor $ Var flex (toSuper name) (Just name)


toSuper :: String -> Maybe Super
toSuper name =
  if List.isPrefixOf "number" name then
      Just Number

  else if List.isPrefixOf "comparable" name then
      Just Comparable

  else if List.isPrefixOf "appendable" name then
      Just Appendable

  else
      Nothing



-- CONSTRAINT HELPERS


monoscheme :: Map.Map String (A.Located a) -> Scheme a b
monoscheme headers =
  Scheme [] [] CTrue headers


infixl 8 /\

(/\) :: Constraint a b -> Constraint a b -> Constraint a b
(/\) c1 c2 =
    case (c1, c2) of
      (CTrue, _) -> c2
      (_, CTrue) -> c1
      _ -> CAnd [c1,c2]


-- ex qs constraint == exists qs. constraint
ex :: [Variable] -> TypeConstraint -> TypeConstraint
ex fqs constraint =
    CLet [Scheme [] fqs constraint Map.empty] CTrue


forall :: [Variable] -> TypeConstraint -> TypeConstraint
forall rqs constraint =
    CLet [Scheme rqs [] constraint Map.empty] CTrue


exists :: (Type -> IO TypeConstraint) -> IO TypeConstraint
exists f =
  do  v <- mkVar Nothing
      ex [v] <$> f (VarN v)


existsNumber :: (Type -> IO TypeConstraint) -> IO TypeConstraint
existsNumber f =
  do  v <- mkVar (Just Number)
      ex [v] <$> f (VarN v)



-- CONVERT TO SOURCE TYPES


-- TODO: Attach resulting type to the descriptor so that you
-- never have to do extra work, particularly nice for aliased types
toSrcType :: Variable -> IO T.Canonical
toSrcType variable =
  do  takenNames <- State.execStateT (getVarNames variable) Map.empty
      State.evalStateT (variableToSrcType variable) (makeNameState takenNames)


variableToSrcType :: Variable -> StateT NameState IO T.Canonical
variableToSrcType variable =
  do  descriptor <- liftIO $ UF.descriptor variable
      let mark = _mark descriptor
      if mark == occursMark
        then
          return (T.Var "∞")

        else
          do  liftIO $ UF.modifyDescriptor variable (\desc -> desc { _mark = occursMark })
              srcType <- contentToSrcType variable (_content descriptor)
              liftIO $ UF.modifyDescriptor variable (\desc -> desc { _mark = mark })
              return srcType


contentToSrcType :: Variable -> Content -> StateT NameState IO T.Canonical
contentToSrcType variable content =
  case content of
    Structure term ->
        termToSrcType term

    Atom name ->
        return (T.Type name)

    Var _ _ (Just name) ->
        return (T.Var name)

    Var flex maybeSuper Nothing ->
        do  freshName <- getFreshName maybeSuper
            liftIO $ UF.modifyDescriptor variable $ \desc ->
                desc { _content = Var flex maybeSuper (Just freshName) }
            return (T.Var freshName)

    Alias name args realVariable ->
        do  srcArgs <- traverse (traverse variableToSrcType) args
            srcType <- variableToSrcType realVariable
            return (T.Aliased name srcArgs (T.Filled srcType))

    Error ->
        return (T.Var "?")


termToSrcType :: Term1 Variable -> StateT NameState IO T.Canonical
termToSrcType term =
  case term of
    App1 func arg ->
      do  srcFunc <- variableToSrcType func
          srcArg <- variableToSrcType arg
          case srcFunc of
            T.App f args ->
              return (T.App f (args ++ [srcArg]))

            _ ->
              return (T.App srcFunc [srcArg])

    Fun1 a b ->
      T.Lambda
        <$> variableToSrcType a
        <*> variableToSrcType b

    EmptyRecord1 ->
      return $ T.Record [] Nothing

    Record1 fields extension ->
      do  srcFields <- Map.toList <$> traverse variableToSrcType fields
          srcExt <- T.iteratedDealias <$> variableToSrcType extension
          return $
              case srcExt of
                T.Record subFields subExt ->
                    T.Record (subFields ++ srcFields) subExt

                T.Var _ ->
                    T.Record srcFields (Just srcExt)

                _ ->
                    error "Used toSrcType on a type that is not well-formed"



-- MANAGE FRESH VARIABLE NAMES


data NameState = NameState
    { _taken :: TakenNames
    , _normals :: Int
    , _numbers :: Int
    , _comparables :: Int
    , _appendables :: Int
    , _compAppends :: Int
    }


type TakenNames = Map.Map String Variable


makeNameState :: TakenNames -> NameState
makeNameState taken =
  NameState taken 0 0 0 0 0


getFreshName :: (Monad m) => Maybe Super -> StateT NameState m String
getFreshName maybeSuper =
  case maybeSuper of
    Nothing ->
      do  index <- State.gets _normals
          taken <- State.gets _taken
          let (uniqueName, newIndex) = getFreshNormal index taken
          State.modify (\state -> state { _normals = newIndex })
          return uniqueName

    Just Number ->
        getFreshSuper "number" _numbers (\index state -> state { _numbers = index })

    Just Comparable ->
        getFreshSuper "comparable" _comparables (\index state -> state { _comparables = index })

    Just Appendable ->
        getFreshSuper "appendable" _appendables (\index state -> state { _appendables = index })

    Just CompAppend ->
        getFreshSuper "compappend" _compAppends (\index state -> state { _compAppends = index })


getFreshNormal :: Int -> TakenNames -> (String, Int)
getFreshNormal index taken =
  let
    (postfix, letter) =
      quotRem index 26

    name =
      Char.chr (97 + letter) : if postfix <= 0 then "" else show postfix
  in
    if Map.member name taken then
      getFreshNormal (index + 1) taken

    else
      (name, index + 1)


getFreshSuper
    :: (Monad m)
    => String
    -> (NameState -> Int)
    -> (Int -> NameState -> NameState)
    -> StateT NameState m String
getFreshSuper name getter setter =
  do  index <- State.gets getter
      taken <- State.gets _taken
      let (uniqueName, newIndex) = getFreshSuperHelp name index taken
      State.modify (setter newIndex)
      return uniqueName


getFreshSuperHelp :: String -> Int -> TakenNames -> (String, Int)
getFreshSuperHelp name index taken =
  let
    newName =
      if index <= 0 then name else name ++ show index
  in
    if Map.member newName taken then
      getFreshSuperHelp name (index + 1) taken

    else
      (newName, index + 1)



-- GET ALL VARIABLE NAMES


getVarNames :: Variable -> StateT TakenNames IO ()
getVarNames var =
  do  desc <- liftIO $ UF.descriptor var
      case _mark desc == getVarNamesMark of
        True ->
          return ()

        False ->
          do  liftIO $ UF.setDescriptor var (desc { _mark = getVarNamesMark })
              case _content desc of
                Atom _ ->
                  return ()

                Error ->
                  return ()

                Var _ _ Nothing ->
                  return ()

                Var flex maybeSuper (Just name) ->
                  do  oldTaken <- State.get
                      newTaken <- liftIO $ addVarName 0 name var flex maybeSuper oldTaken
                      State.put newTaken

                Alias _ args realVar ->
                  do  mapM_ (getVarNames . snd) args
                      getVarNames realVar

                Structure (App1 func arg) ->
                  do  getVarNames func
                      getVarNames arg

                Structure (Fun1 arg body) ->
                  do  getVarNames arg
                      getVarNames body

                Structure EmptyRecord1 ->
                  return ()

                Structure (Record1 fields extension) ->
                  do  mapM_ getVarNames fields
                      getVarNames extension


addVarName :: Int -> String -> Variable -> Flex -> Maybe Super -> TakenNames -> IO TakenNames
addVarName index givenName var flex maybeSuper taken =
  let
    name =
      makeIndexedName givenName index
  in
    case Map.lookup name taken of
      Nothing ->
        do  when (name /= givenName) $ UF.modifyDescriptor var $ \desc ->
              desc { _content = Var flex maybeSuper (Just name) }
            return $ Map.insert name var taken

      Just otherVar ->
        do  same <- UF.equivalent var otherVar
            if same
              then return taken
              else addVarName (index + 1) givenName var flex maybeSuper taken


makeIndexedName :: String -> Int -> String
makeIndexedName name index =
  if index <= 0 then
    name

  else if Char.isDigit (last name) then
    name ++ '_' : show index

  else
    name ++ show index
