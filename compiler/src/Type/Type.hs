{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Type.Type
  ( Constraint(..)
  , exists
  , Variable
  , FlatType(..)
  , Type(..)
  , Descriptor(Descriptor)
  , Content(..)
  , SuperType(..)
  , noRank
  , outermostRank
  , Mark
  , noMark
  , nextMark
  , (==>)
  , int, float, char, string, bool, never
  , vec2, vec3, vec4, mat4, texture
  , mkFlexVar
  , mkFlexNumber
  , unnamedFlexVar
  , unnamedFlexSuper
  , nameToFlex
  , nameToRigid
  , toAnnotation
  , toErrorType
  )
  where


import Control.Monad.State.Strict (StateT, liftIO)
import qualified Control.Monad.State.Strict as State
import Data.Foldable (foldrM)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)

import qualified Crash

import qualified AST.Canonical as Can
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified AST.Utils.Type as Type
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as E
import qualified Type.Error as ET
import qualified Type.UnionFind as UF



-- CONSTRAINTS


data Constraint
  = CTrue
  | CSaveTheEnvironment
  | CEqual A.Region E.Category Type (E.Expected Type)
  | CLocal A.Region N.Name (E.Expected Type)
  | CForeign A.Region N.Name Can.Annotation (E.Expected Type)
  | COperator A.Region Op.Name Can.Annotation (E.Expected Type)
  | CPattern A.Region E.PCategory Type (E.PExpected Type)
  | CAnd [Constraint]
  | CLet
      { _rigidVars :: [Variable]
      , _flexVars :: [Variable]
      , _header :: Map.Map N.Name (A.Located Type)
      , _headerCon :: Constraint
      , _bodyCon :: Constraint
      }


exists :: [Variable] -> Constraint -> Constraint
exists flexVars constraint =
  CLet [] flexVars Map.empty constraint CTrue



-- TYPE PRIMITIVES
--
-- TODO is PlaceHolder really unused now?


type Variable =
    UF.Point Descriptor


data FlatType
    = App1 ModuleName.Canonical T.Name [Variable]
    | Fun1 Variable Variable
    | EmptyRecord1
    | Record1 (Map.Map N.Name Variable) Variable
    | Unit1
    | Tuple1 Variable Variable (Maybe Variable)


data Type
    = PlaceHolder T.Var
    | AliasN ModuleName.Canonical T.Name [(T.Var, Type)] Type
    | VarN Variable
    | AppN ModuleName.Canonical T.Name [Type]
    | FunN Type Type
    | EmptyRecordN
    | RecordN (Map.Map N.Name Type) Type
    | UnitN
    | TupleN Type Type (Maybe Type)



-- DESCRIPTORS


data Descriptor =
  Descriptor
    { _content :: Content
    , _rank :: Int
    , _mark :: Mark
    , _copy :: Maybe Variable
    }


data Content
    = FlexVar (Maybe T.Var)
    | FlexSuper SuperType (Maybe T.Var)
    | RigidVar T.Var
    | RigidSuper SuperType T.Var
    | Structure FlatType
    | Alias ModuleName.Canonical T.Name [(T.Var,Variable)] Variable
    | Error


data SuperType
  = Number
  | Comparable
  | Appendable
  | CompAppend
  deriving (Eq)


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



-- FUNCTION TYPES


infixr 9 ==>


{-# INLINE (==>) #-}
(==>) :: Type -> Type -> Type
(==>) =
  FunN



-- PRIMITIVE TYPES


{-# NOINLINE int    #-}; int    :: Type; int    = AppN ModuleName.basics T.int    []
{-# NOINLINE float  #-}; float  :: Type; float  = AppN ModuleName.basics T.float  []
{-# NOINLINE char   #-}; char   :: Type; char   = AppN ModuleName.char   T.char   []
{-# NOINLINE string #-}; string :: Type; string = AppN ModuleName.string T.string []
{-# NOINLINE bool   #-}; bool   :: Type; bool   = AppN ModuleName.basics T.bool   []
{-# NOINLINE never  #-}; never  :: Type; never  = AppN ModuleName.basics T.never  []



-- WEBGL TYPES


{-# NOINLINE vec2    #-}; vec2    :: Type; vec2    = AppN ModuleName.vector2 T.vec2    []
{-# NOINLINE vec3    #-}; vec3    :: Type; vec3    = AppN ModuleName.vector3 T.vec3    []
{-# NOINLINE vec4    #-}; vec4    :: Type; vec4    = AppN ModuleName.vector4 T.vec4    []
{-# NOINLINE mat4    #-}; mat4    :: Type; mat4    = AppN ModuleName.matrix4 T.mat4    []
{-# NOINLINE texture #-}; texture :: Type; texture = AppN ModuleName.texture T.texture []



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
  makeDescriptor (unnamedFlexSuper Number)


unnamedFlexSuper :: SuperType -> Content
unnamedFlexSuper super =
  FlexSuper super Nothing



-- MAKE NAMED VARIABLES


nameToFlex :: T.Var -> IO Variable
nameToFlex name =
  UF.fresh $ makeDescriptor $
    maybe FlexVar FlexSuper (toSuper name) (Just name)


nameToRigid :: T.Var -> IO Variable
nameToRigid name =
  UF.fresh $ makeDescriptor $
    maybe RigidVar RigidSuper (toSuper name) name


toSuper :: T.Var -> Maybe SuperType
toSuper (T.Var _ c) =
  case c of
    T.Any        -> Nothing
    T.Comparable -> Just Comparable
    T.Appendable -> Just Appendable
    T.CompAppend -> Just CompAppend
    T.Number     -> Just Number



-- TO TYPE ANNOTATION


toAnnotation :: Variable -> IO Can.Annotation
toAnnotation variable =
  do  userNames <- getVarNames variable Map.empty
      (tipe, NameState freeVars _ _ _ _ _) <-
        State.runStateT (variableToCanType variable) (makeNameState userNames)
      return $ Can.Forall freeVars tipe


variableToCanType :: Variable -> StateT NameState IO Can.Type
variableToCanType variable =
  do  (Descriptor content _ _ _) <- liftIO $ UF.get variable
      case content of
        Structure term ->
            termToCanType term

        FlexVar maybeName ->
          case maybeName of
            Just name ->
              return (Can.TVar name)

            Nothing ->
              do  name <- getFreshVarName
                  liftIO $ UF.modify variable (\desc -> desc { _content = FlexVar (Just name) })
                  return (Can.TVar name)

        FlexSuper super maybeName ->
          case maybeName of
            Just name ->
              return (Can.TVar name)

            Nothing ->
              do  name <- getFreshSuperName super
                  liftIO $ UF.modify variable (\desc -> desc { _content = FlexSuper super (Just name) })
                  return (Can.TVar name)

        RigidVar name ->
            return (Can.TVar name)

        RigidSuper _ name ->
            return (Can.TVar name)

        Alias home name args realVariable ->
            do  canArgs <- traverse (traverse variableToCanType) args
                canType <- variableToCanType realVariable
                return (Can.TAlias home name canArgs (Can.Filled canType))

        Error ->
            $(Crash.crash 'variableToCanType) "cannot handle Error types in variableToCanType"


termToCanType :: FlatType -> StateT NameState IO Can.Type
termToCanType term =
  case term of
    App1 home name args ->
      Can.TType home name <$> traverse variableToCanType args

    Fun1 a b ->
      Can.TLambda
        <$> variableToCanType a
        <*> variableToCanType b

    EmptyRecord1 ->
      return $ Can.TRecord Map.empty Nothing

    Record1 fields extension ->
      do  canFields <- traverse fieldToCanType fields
          canExt <- Type.iteratedDealias <$> variableToCanType extension
          return $
              case canExt of
                Can.TRecord subFields subExt ->
                    Can.TRecord (Map.union subFields canFields) subExt

                Can.TVar name ->
                    Can.TRecord canFields (Just name)

                _ ->
                    $(Crash.crash 'termToCanType) "Used toAnnotation on a type that is not well-formed"

    Unit1 ->
      return Can.TUnit

    Tuple1 a b maybeC ->
      Can.TTuple
        <$> variableToCanType a
        <*> variableToCanType b
        <*> traverse variableToCanType maybeC


fieldToCanType :: Variable -> StateT NameState IO Can.FieldType
fieldToCanType variable =
  do  tipe <- variableToCanType variable
      return (Can.FieldType 0 tipe)



-- TO ERROR TYPE


toErrorType :: Variable -> IO ET.Type
toErrorType variable =
  do  userNames <- getVarNames variable Map.empty
      State.evalStateT (variableToErrorType variable) (makeNameState userNames)


variableToErrorType :: Variable -> StateT NameState IO ET.Type
variableToErrorType variable =
  do  descriptor <- liftIO $ UF.get variable
      let mark = _mark descriptor
      if mark == occursMark
        then
          return ET.Infinite

        else
          do  liftIO $ UF.modify variable (\desc -> desc { _mark = occursMark })
              errType <- contentToErrorType variable (_content descriptor)
              liftIO $ UF.modify variable (\desc -> desc { _mark = mark })
              return errType


contentToErrorType :: Variable -> Content -> StateT NameState IO ET.Type
contentToErrorType variable content =
  case content of
    Structure term ->
        termToErrorType term

    FlexVar maybeName ->
      case maybeName of
        Just name ->
          return (ET.FlexVar name)

        Nothing ->
          do  name <- getFreshVarName
              liftIO $ UF.modify variable (\desc -> desc { _content = FlexVar (Just name) })
              return (ET.FlexVar name)

    FlexSuper super maybeName ->
      case maybeName of
        Just name ->
          return (ET.FlexSuper (superToSuper super) name)

        Nothing ->
          do  name <- getFreshSuperName super
              liftIO $ UF.modify variable (\desc -> desc { _content = FlexSuper super (Just name) })
              return (ET.FlexSuper (superToSuper super) name)

    RigidVar name ->
        return (ET.RigidVar name)

    RigidSuper super name ->
        return (ET.RigidSuper (superToSuper super) name)

    Alias home name args realVariable ->
        do  errArgs <- traverse (traverse variableToErrorType) args
            errType <- variableToErrorType realVariable
            return (ET.Alias home name errArgs errType)

    Error ->
        return ET.Error


superToSuper :: SuperType -> ET.Super
superToSuper super =
  case super of
    Number -> ET.Number
    Comparable -> ET.Comparable
    Appendable -> ET.Appendable
    CompAppend -> ET.CompAppend


termToErrorType :: FlatType -> StateT NameState IO ET.Type
termToErrorType term =
  case term of
    App1 home name args ->
      ET.Type home name <$> traverse variableToErrorType args

    Fun1 a b ->
      do  arg <- variableToErrorType a
          result <- variableToErrorType b
          return $
            case result of
              ET.Lambda arg1 arg2 others ->
                ET.Lambda arg arg1 (arg2:others)

              _ ->
                ET.Lambda arg result []

    EmptyRecord1 ->
      return $ ET.Record Map.empty ET.Closed

    Record1 fields extension ->
      do  errFields <- traverse variableToErrorType fields
          errExt <- ET.iteratedDealias <$> variableToErrorType extension
          return $
              case errExt of
                ET.Record subFields subExt ->
                    ET.Record (Map.union subFields errFields) subExt

                ET.FlexVar ext ->
                    ET.Record errFields (ET.FlexOpen ext)

                ET.RigidVar ext ->
                    ET.Record errFields (ET.RigidOpen ext)

                _ ->
                    $(Crash.crash 'termToErrorType) "Used toErrorType on a type that is not well-formed"

    Unit1 ->
      return ET.Unit

    Tuple1 a b maybeC ->
      case maybeC of
        Nothing -> ET.Pair   <$> variableToErrorType a <*> variableToErrorType b
        Just c  -> ET.Triple <$> variableToErrorType a <*> variableToErrorType b <*> variableToErrorType c



-- MANAGE FRESH VARIABLE NAMES


data NameState =
  NameState
    { _taken :: Map.Map T.Var ()
    , _normals :: Int
    , _numbers :: Int
    , _comparables :: Int
    , _appendables :: Int
    , _compAppends :: Int
    }


makeNameState :: Map.Map T.Var Variable -> NameState
makeNameState taken =
  NameState (Map.map (const ()) taken) 0 0 0 0 0



-- FRESH VAR NAMES


getFreshVarName :: (Monad m) => StateT NameState m T.Var
getFreshVarName =
  do  index <- State.gets _normals
      taken <- State.gets _taken
      let (name, newIndex, newTaken) = getFreshVarNameHelp index taken
      State.modify $ \state -> state { _taken = newTaken, _normals = newIndex }
      return name


getFreshVarNameHelp :: Int -> Map.Map T.Var () -> (T.Var, Int, Map.Map T.Var ())
getFreshVarNameHelp index taken =
  let
    name = T.genAny index
  in
  if Map.member name taken then
    getFreshVarNameHelp (index + 1) taken
  else
    ( name, index + 1, Map.insert name () taken )



-- FRESH SUPER NAMES


getFreshSuperName :: (Monad m) => SuperType -> StateT NameState m T.Var
getFreshSuperName super =
  case super of
    Number     -> getFreshSuper T.genNumber     _numbers     (\i s -> s { _numbers     = i })
    Comparable -> getFreshSuper T.genComparable _comparables (\i s -> s { _comparables = i })
    Appendable -> getFreshSuper T.genAppendable _appendables (\i s -> s { _appendables = i })
    CompAppend -> getFreshSuper T.genCompAppend _compAppends (\i s -> s { _compAppends = i })


getFreshSuper :: (Monad m) => (Int -> T.Var) -> (NameState -> Int) -> (Int -> NameState -> NameState) -> StateT NameState m T.Var
getFreshSuper gen getter setter =
  do  index <- State.gets getter
      taken <- State.gets _taken
      let (name, newIndex, newTaken) = getFreshSuperHelp gen index taken
      State.modify (\state -> setter newIndex state { _taken = newTaken })
      return name


getFreshSuperHelp :: (Int -> T.Var) -> Int -> Map.Map T.Var () -> (T.Var, Int, Map.Map T.Var ())
getFreshSuperHelp gen index taken =
  let
    name = gen index
  in
    if Map.member name taken then
      getFreshSuperHelp gen (index + 1) taken

    else
      ( name, index + 1, Map.insert name () taken )



-- GET ALL VARIABLE NAMES


getVarNames :: Variable -> Map.Map T.Var Variable -> IO (Map.Map T.Var Variable)
getVarNames var takenNames =
  do  (Descriptor content rank mark copy) <- UF.get var
      if mark == getVarNamesMark
        then return takenNames
        else
        do  UF.set var (Descriptor content rank getVarNamesMark copy)
            case content of
              Error ->
                return takenNames

              FlexVar maybeName ->
                case maybeName of
                  Nothing   -> return takenNames
                  Just name -> addName 0 name var (FlexVar . Just) takenNames

              FlexSuper super maybeName ->
                case maybeName of
                  Nothing   -> return takenNames
                  Just name -> addName 0 name var (FlexSuper super . Just) takenNames

              RigidVar name ->
                addName 0 name var RigidVar takenNames

              RigidSuper super name ->
                addName 0 name var (RigidSuper super) takenNames

              Alias _ _ args _ ->
                foldrM getVarNames takenNames (map snd args)

              Structure flatType ->
                case flatType of
                  App1 _ _ xs         -> foldrM getVarNames takenNames xs
                  Fun1 x e            -> getVarNames x =<< getVarNames e takenNames
                  EmptyRecord1        -> return takenNames
                  Record1 fs x        -> getVarNames x =<< foldrM getVarNames takenNames (Map.elems fs)
                  Unit1               -> return takenNames
                  Tuple1 a b Nothing  -> getVarNames a =<< getVarNames b takenNames
                  Tuple1 a b (Just c) -> getVarNames a =<< getVarNames b =<< getVarNames c takenNames



-- REGISTER NAME / RENAME DUPLICATES


addName :: Int -> T.Var -> Variable -> (T.Var -> Content) -> Map.Map T.Var Variable -> IO (Map.Map T.Var Variable)
addName index givenName@(T.Var v c) var makeContent takenNames =
  let
    indexedName =
      T.Var (T.genIndexed v index) c
  in
    case Map.lookup indexedName takenNames of
      Nothing ->
        do  if indexedName == givenName then return () else
              UF.modify var $ \(Descriptor _ rank mark copy) ->
                Descriptor (makeContent indexedName) rank mark copy
            return $ Map.insert indexedName var takenNames

      Just otherVar ->
        do  same <- UF.equivalent var otherVar
            if same
              then return takenNames
              else addName (index + 1) givenName var makeContent takenNames

