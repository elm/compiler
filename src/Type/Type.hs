{-# OPTIONS_GHC -Wall #-}
module Type.Type where

import Control.Monad.State (StateT, liftIO)
import qualified Control.Monad.State as State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Traversable as Traverse (traverse)
import qualified Data.UnionFind.IO as UF

import qualified AST.Module.Name as ModuleName
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


mkNamedVar :: String -> IO Variable
mkNamedVar name =
    UF.fresh $ mkDescriptor (Var Flex (toSuper name) Nothing)


mkRigid :: String -> IO Variable
mkRigid name =
    UF.fresh $ mkDescriptor (Var Rigid (toSuper name) (Just name))


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


-- fl qs constraint == forall qs. constraint
fl :: [Variable] -> TypeConstraint -> TypeConstraint
fl rqs constraint =
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
  do  usedNames <- getVarNames variable
      State.evalStateT (variableToSrcType variable) (makeNameState usedNames)


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
        do  srcArgs <- mapM (\(arg,tvar) -> (,) arg <$> variableToSrcType tvar) args
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
      do  srcFields <- Map.toList <$> Traverse.traverse variableToSrcType fields
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
    { _freeNames :: [String]
    , _numberPrimes :: Int
    , _comparablePrimes :: Int
    , _appendablePrimes :: Int
    , _compAppendPrimes :: Int
    }


makeNameState :: Set.Set String -> NameState
makeNameState usedNames =
  let
    makeName suffix =
      map (:suffix) ['a'..'z']

    allNames =
      concatMap makeName ("" : map show [ (1 :: Int) .. ])

    freeNames =
      filter (\name -> Set.notMember name usedNames) allNames
  in
    NameState freeNames 0 0 0 0


getFreshName :: (Monad m) => Maybe Super -> StateT NameState m String
getFreshName maybeSuper =
  case maybeSuper of
    Nothing ->
        do  names <- State.gets _freeNames
            State.modify (\state -> state { _freeNames = tail names })
            return (head names)

    Just Number ->
        do  primes <- State.gets _numberPrimes
            State.modify (\state -> state { _numberPrimes = primes + 1 })
            return ("number" ++ replicate primes '\'')

    Just Comparable ->
        do  primes <- State.gets _comparablePrimes
            State.modify (\state -> state { _comparablePrimes = primes + 1 })
            return ("comparable" ++ replicate primes '\'')

    Just Appendable ->
        do  primes <- State.gets _appendablePrimes
            State.modify (\state -> state { _appendablePrimes = primes + 1 })
            return ("appendable" ++ replicate primes '\'')

    Just CompAppend ->
        do  primes <- State.gets _compAppendPrimes
            State.modify (\state -> state { _compAppendPrimes = primes + 1 })
            return ("compappend" ++ replicate primes '\'')



-- GET ALL VARIABLE NAMES


getVarNames :: Variable -> IO (Set.Set String)
getVarNames var =
  do  desc <- UF.descriptor var
      if _mark desc == getVarNamesMark
        then
          return Set.empty

        else
          do  UF.setDescriptor var (desc { _mark = getVarNamesMark })
              getVarNamesHelp (_content desc)


getVarNamesHelp :: Content -> IO (Set.Set String)
getVarNamesHelp content =
  case content of
    Var _ _ (Just name) ->
        return (Set.singleton name)

    Var _ _ Nothing ->
        return Set.empty

    Structure term ->
        getVarNamesTerm term

    Alias _name args realVar ->
        do  let argSet = Set.fromList (map fst args)
            realSet <- getVarNames realVar
            sets <- mapM (getVarNames . snd) args
            return (Set.unions (realSet : argSet : sets))

    Atom _ ->
        return Set.empty

    Error ->
        return Set.empty


getVarNamesTerm :: Term1 Variable -> IO (Set.Set String)
getVarNamesTerm term =
  let go = getVarNames in
  case term of
    App1 a b ->
        Set.union <$> go a <*> go b

    Fun1 a b ->
        Set.union <$> go a <*> go b

    EmptyRecord1 ->
        return Set.empty

    Record1 fields extension ->
        do  fieldVars <- Set.unions <$> mapM go (Map.elems fields)
            Set.union fieldVars <$> go extension
