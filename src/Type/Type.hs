{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Type
  ( Variable
  , FlatType(..)
  , Type(..)
  , Descriptor(Descriptor)
  , Content(..)
  , noRank
  , outermostRank
  , Mark
  , noMark
  , nextMark
  , (==>), float, char, string, bool
  , mkFlexVar
  , mkFlexNumber
  , unnamedFlexVar
  , unnamedFlexSuper
  , nameToFlex
  , nameToRigid
  , toSrcType
  , fromFlexSrcType
  , fromRigidSrcType
  )
  where

import Control.Monad.State.Strict (StateT, liftIO)
import qualified Control.Monad.State.Strict as State
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Word (Word32)

import qualified AST.Module.Name as ModuleName
import qualified AST.Type as T
import qualified Elm.Name as N
import qualified Type.UnionFind as UF



-- TYPE PRIMITIVES


type Variable =
    UF.Point Descriptor


data FlatType
    = App1 ModuleName.Canonical N.Name [Variable]
    | Fun1 Variable Variable
    | EmptyRecord1
    | Record1 (Map.Map Text Variable) Variable
    | Unit1
    | Tuple1 Variable Variable [Variable]


data Type
    = PlaceHolder Text
    | AliasN ModuleName.Canonical N.Name [(Text, Type)] Type
    | VarN Variable
    | AppN ModuleName.Canonical N.Name [Type]
    | FunN Type Type
    | EmptyRecordN
    | RecordN (Map.Map Text Type) Type
    | UnitN
    | TupleN Type Type [Type]



-- DESCRIPTORS


data Descriptor =
  Descriptor
    { _content :: Content
    , _rank :: Int
    , _mark :: Mark
    , _copy :: Maybe Variable
    }


data Content
    = FlexVar (Maybe Text)
    | FlexSuper T.Super (Maybe Text)
    | RigidVar Text
    | RigidSuper T.Super Text
    | Structure FlatType
    | Alias ModuleName.Canonical N.Name [(Text,Variable)] Variable
    | Error Text


makeDescriptor :: Content -> Descriptor
makeDescriptor content =
  Descriptor content noRank noMark Nothing



-- RANKS


noRank :: Int
noRank =
  0


outermostRank :: Int
outermostRank =
  1



-- MARKS


newtype Mark = Mark Word32
  deriving (Eq, Ord)


noMark :: Mark
noMark =
  Mark 2


occursMark :: Mark
occursMark =
  Mark 1


getVarNamesMark :: Mark
getVarNamesMark =
  Mark 0


{-# INLINE nextMark #-}
nextMark :: Mark -> Mark
nextMark (Mark mark) =
  Mark (mark + 1)



-- TYPE HELPERS


infixr 9 ==>


{-# INLINE (==>) #-}
(==>) :: Type -> Type -> Type
(==>) =
  FunN


{-# NOINLINE float #-}
float :: Type
float =
  AppN ModuleName.basics "Float" []


{-# NOINLINE char #-}
char :: Type
char =
  AppN ModuleName.basics "Char" []


{-# NOINLINE string #-}
string :: Type
string =
  AppN ModuleName.basics "String" []


{-# NOINLINE bool #-}
bool :: Type
bool =
  AppN ModuleName.basics "Bool" []



-- MAKE FLEX VARIABLES


mkFlexVar :: IO Variable
mkFlexVar =
  UF.fresh flexVarDescriptor


{-# NOINLINE flexVarDescriptor #-}
flexVarDescriptor :: Descriptor
flexVarDescriptor =
  makeDescriptor unnamedFlexVar


{-# NOINLINE unnamedFlexVar #-}
unnamedFlexVar :: Content
unnamedFlexVar =
  FlexVar Nothing



-- MAKE FLEX NUMBERS


mkFlexNumber :: IO Variable
mkFlexNumber =
  UF.fresh flexNumberDescriptor


{-# NOINLINE flexNumberDescriptor #-}
flexNumberDescriptor :: Descriptor
flexNumberDescriptor =
  makeDescriptor (unnamedFlexSuper T.Number)


unnamedFlexSuper :: T.Super -> Content
unnamedFlexSuper super =
  FlexSuper super Nothing



-- MAKE NAMED VARIABLES


nameToFlex :: N.Name -> IO Variable
nameToFlex name =
  UF.fresh $ makeDescriptor $
    maybe FlexVar FlexSuper (toSuper name) (Just name)


nameToRigid :: N.Name -> IO Variable
nameToRigid name =
  UF.fresh $ makeDescriptor $
    maybe RigidVar RigidSuper (toSuper name) name


toSuper :: N.Name -> Maybe T.Super
toSuper name =
  if Text.isPrefixOf "number" name then
      Just T.Number

  else if Text.isPrefixOf "comparable" name then
      Just T.Comparable

  else if Text.isPrefixOf "appendable" name then
      Just T.Appendable

  else if Text.isPrefixOf "compappend" name then
      Just T.CompAppend

  else
      Nothing



-- FROM SOURCE TYPES


-- TODO should the freeFlexVars be ranked a certain way?
-- How does this interact with `TS.flatten` exactly?
fromFlexSrcType :: T.Canonical -> IO ( Type, Map.Map N.Name Variable )
fromFlexSrcType srcType =
  do  let freeVars = gatherFreeVars srcType Map.empty
      freeFlexVars <- Map.traverseWithKey (\name () -> nameToFlex name) freeVars
      tipe <- fromSrcType (Map.map VarN freeFlexVars) srcType
      return ( tipe, freeFlexVars )


fromRigidSrcType :: T.Canonical -> IO ( Type, Map.Map N.Name Variable )
fromRigidSrcType srcType =
  do  let freeVars = gatherFreeVars srcType Map.empty
      freeRigidVars <- Map.traverseWithKey (\name () -> nameToRigid name) freeVars
      tipe <- fromSrcType (Map.map VarN freeRigidVars) srcType
      return ( tipe, freeRigidVars )


gatherFreeVars :: T.Canonical -> Map.Map N.Name () -> Map.Map N.Name ()
gatherFreeVars tipe dict =
  case tipe of
    T.Lambda arg result ->
      gatherFreeVars result (gatherFreeVars arg dict)

    T.Var name ->
      Map.insert name () dict

    T.Type _ _ args ->
      foldr gatherFreeVars dict args

    T.Aliased _ _ args _ ->
      foldr gatherFreeVars dict (map snd args)

    T.Tuple a b cs ->
      gatherFreeVars a $ gatherFreeVars b $
        foldr gatherFreeVars dict cs

    T.Unit ->
      dict

    T.Record fields maybeExt ->
      case maybeExt of
        Nothing ->
          Map.foldr gatherFreeVars dict fields

        Just ext ->
          Map.foldr gatherFreeVars (gatherFreeVars ext dict) fields


fromSrcType :: Map.Map N.Name Type -> T.Canonical -> IO Type
fromSrcType freeVars sourceType =
  case sourceType of
    T.Lambda arg result ->
      FunN
        <$> fromSrcType freeVars arg
        <*> fromSrcType freeVars result

    T.Var name ->
      return (freeVars ! name)

    T.Type home name args ->
      AppN home name <$> traverse (fromSrcType freeVars) args

    T.Aliased home name args aliasedType ->
      do  targs <- traverse (traverse (fromSrcType freeVars)) args
          AliasN home name targs <$>
            case aliasedType of
              T.Filled realType ->
                fromSrcType freeVars realType

              T.Holey realType ->
                fromSrcType (Map.fromList targs) realType

    T.Tuple a b cs ->
      TupleN
        <$> fromSrcType freeVars a
        <*> fromSrcType freeVars b
        <*> traverse (fromSrcType freeVars) cs

    T.Unit ->
      return UnitN

    T.Record fields maybeExt ->
      RecordN
        <$> traverse (fromSrcType freeVars) fields
        <*>
          case maybeExt of
            Nothing ->
              return EmptyRecordN

            Just ext ->
              fromSrcType freeVars ext



-- TO SOURCE TYPES


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

    FlexVar maybeName ->
      case maybeName of
        Just name ->
          return (T.Var name)

        Nothing ->
          do  name <- getFreshVarName
              liftIO $ UF.modifyDescriptor variable (\desc -> desc { _content = FlexVar (Just name) })
              return (T.Var name)

    FlexSuper super maybeName ->
      case maybeName of
        Just name ->
          return (T.Var name)

        Nothing ->
          do  name <- getFreshSuperName super
              liftIO $ UF.modifyDescriptor variable (\desc -> desc { _content = FlexSuper super (Just name) })
              return (T.Var name)

    RigidVar name ->
        return (T.Var name)

    RigidSuper _ name ->
        return (T.Var name)

    Alias home name args realVariable ->
        do  srcArgs <- traverse (traverse variableToSrcType) args
            srcType <- variableToSrcType realVariable
            return (T.Aliased home name srcArgs (T.Filled srcType))

    Error name ->
        return (T.Var name)


termToSrcType :: FlatType -> StateT NameState IO T.Canonical
termToSrcType term =
  case term of
    App1 home name args ->
      T.Type home name <$> traverse variableToSrcType args

    Fun1 a b ->
      T.Lambda
        <$> variableToSrcType a
        <*> variableToSrcType b

    EmptyRecord1 ->
      return $ T.Record Map.empty Nothing

    Record1 fields extension ->
      do  srcFields <- traverse variableToSrcType fields
          srcExt <- T.iteratedDealias <$> variableToSrcType extension
          return $
              case srcExt of
                T.Record subFields subExt ->
                    T.Record (Map.union subFields srcFields) subExt

                T.Var _ ->
                    T.Record srcFields (Just srcExt)

                _ ->
                    error "Used toSrcType on a type that is not well-formed"

    Unit1 ->
      return T.Unit

    Tuple1 a b cs ->
      T.Tuple
        <$> variableToSrcType a
        <*> variableToSrcType b
        <*> traverse variableToSrcType cs



-- MANAGE FRESH VARIABLE NAMES


data NameState =
  NameState
    { _taken :: TakenNames
    , _normals :: Int
    , _numbers :: Int
    , _comparables :: Int
    , _appendables :: Int
    , _compAppends :: Int
    }


type TakenNames = Map.Map Text Variable


makeNameState :: TakenNames -> NameState
makeNameState taken =
  NameState taken 0 0 0 0 0


getFreshVarName :: (Monad m) => StateT NameState m Text
getFreshVarName =
  do  index <- State.gets _normals
      taken <- State.gets _taken
      let (uniqueName, newIndex) = getFreshNormal index taken
      State.modify (\state -> state { _normals = newIndex })
      return uniqueName


getFreshSuperName :: (Monad m) => T.Super -> StateT NameState m Text
getFreshSuperName super =
  case super of
    T.Number ->
      getFreshSuper "number" _numbers (\index state -> state { _numbers = index })

    T.Comparable ->
      getFreshSuper "comparable" _comparables (\index state -> state { _comparables = index })

    T.Appendable ->
      getFreshSuper "appendable" _appendables (\index state -> state { _appendables = index })

    T.CompAppend ->
      getFreshSuper "compappend" _compAppends (\index state -> state { _compAppends = index })


getFreshNormal :: Int -> TakenNames -> (Text, Int)
getFreshNormal index taken =
  let
    (postfix, letter) =
      quotRem index 26

    character =
      Char.chr (97 + letter)

    name =
      Text.pack (if postfix <= 0 then [character] else character : show postfix)
  in
    if Map.member name taken then
      getFreshNormal (index + 1) taken

    else
      (name, index + 1)


getFreshSuper
    :: (Monad m)
    => Text
    -> (NameState -> Int)
    -> (Int -> NameState -> NameState)
    -> StateT NameState m Text
getFreshSuper name getter setter =
  do  index <- State.gets getter
      taken <- State.gets _taken
      let (uniqueName, newIndex) = getFreshSuperHelp name index taken
      State.modify (setter newIndex)
      return uniqueName


getFreshSuperHelp :: Text -> Int -> TakenNames -> (Text, Int)
getFreshSuperHelp name index taken =
  let
    newName =
      if index <= 0 then name else name <> Text.pack (show index)
  in
    if Map.member newName taken then
      getFreshSuperHelp name (index + 1) taken

    else
      (newName, index + 1)



-- GET ALL VARIABLE NAMES


getVarNames :: Variable -> StateT TakenNames IO ()
getVarNames var =
  do  (Descriptor content rank mark copy) <- liftIO $ UF.descriptor var
      if mark == getVarNamesMark then return () else
        do  liftIO $ UF.setDescriptor var (Descriptor content rank getVarNamesMark copy)
            case content of
              Error _ ->
                return ()

              FlexVar maybeName ->
                case maybeName of
                  Nothing ->
                    return ()

                  Just name ->
                    registerName name var (FlexVar . Just)

              FlexSuper super maybeName ->
                case maybeName of
                  Nothing ->
                    return ()

                  Just name ->
                    registerName name var (FlexSuper super . Just)

              RigidVar name ->
                registerName name var RigidVar

              RigidSuper super name ->
                registerName name var (RigidSuper super)

              Alias _ _ args realVar ->
                do  mapM_ (getVarNames . snd) args
                    getVarNames realVar

              Structure flatType ->
                case flatType of
                  App1 _ _ args ->
                    mapM_ getVarNames args

                  Fun1 arg body ->
                    do  getVarNames arg
                        getVarNames body

                  EmptyRecord1 ->
                    return ()

                  Record1 fields extension ->
                    do  mapM_ getVarNames fields
                        getVarNames extension

                  Unit1 ->
                    return ()

                  Tuple1 a b cs ->
                    do  getVarNames a
                        getVarNames b
                        mapM_ getVarNames cs



-- REGISTER NAME / RENAME DUPLICATES


registerName :: Text -> Variable -> (Text -> Content) -> StateT TakenNames IO ()
registerName givenName var makeContent =
  if Text.null givenName then
    return ()
  else
    do  takenNames <- State.get
        State.put =<< liftIO (checkName 0 givenName var makeContent takenNames)


checkName :: Int -> Text -> Variable -> (Text -> Content) -> TakenNames -> IO TakenNames
checkName index givenName var makeContent takenNames =
  let
    indexedName =
      if index <= 0 then
        givenName
      else if Char.isDigit (Text.last givenName) then
        givenName <> Text.pack ('_' : show index)
      else
        givenName <> Text.pack (show index)
  in
    case Map.lookup indexedName takenNames of
      Nothing ->
        do  if indexedName == givenName then return () else
              UF.modifyDescriptor var $ \(Descriptor _ rank mark copy) ->
                Descriptor (makeContent indexedName) rank mark copy
            return $ Map.insert indexedName var takenNames

      Just otherVar ->
        do  same <- UF.equivalent var otherVar
            if same
              then return takenNames
              else checkName (index + 1) givenName var makeContent takenNames
