{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Type
  ( Variable
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
  , toSrcType
  , toAnnotation
  )
  where


import Control.Monad.State.Strict (StateT, liftIO)
import qualified Control.Monad.State.Strict as State
import Data.Foldable (foldrM)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)

import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Type as Type
import qualified Elm.Name as N
import qualified Type.UnionFind as UF



-- TYPE PRIMITIVES


type Variable =
    UF.Point Descriptor


data FlatType
    = App1 ModuleName.Canonical N.Name [Variable]
    | Fun1 Variable Variable
    | EmptyRecord1
    | Record1 (Map.Map N.Name Variable) Variable
    | Unit1
    | Tuple1 Variable Variable (Maybe Variable)


data Type
    = PlaceHolder N.Name
    | AliasN ModuleName.Canonical N.Name [(N.Name, Type)] Type
    | VarN Variable
    | AppN ModuleName.Canonical N.Name [Type]
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
    = FlexVar (Maybe N.Name)
    | FlexSuper SuperType (Maybe N.Name)
    | RigidVar N.Name
    | RigidSuper SuperType N.Name
    | Structure FlatType
    | Alias ModuleName.Canonical N.Name [(N.Name,Variable)] Variable
    | Error N.Name


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


{-# NOINLINE int #-}
int :: Type
int = AppN ModuleName.basics "Int" []


{-# NOINLINE float #-}
float :: Type
float = AppN ModuleName.basics "Float" []


{-# NOINLINE char #-}
char :: Type
char = AppN ModuleName.char "Char" []


{-# NOINLINE string #-}
string :: Type
string = AppN ModuleName.string "String" []


{-# NOINLINE bool #-}
bool :: Type
bool = AppN ModuleName.basics "Bool" []


{-# NOINLINE never #-}
never :: Type
never = AppN ModuleName.basics "Never" []



-- WEBGL TYPES


{-# NOINLINE vec2 #-}
vec2 :: Type
vec2 = AppN ModuleName.vector2 "Vec2" []


{-# NOINLINE vec3 #-}
vec3 :: Type
vec3 = AppN ModuleName.vector3 "Vec3" []


{-# NOINLINE vec4 #-}
vec4 :: Type
vec4 = AppN ModuleName.vector4 "Vec4" []


{-# NOINLINE mat4 #-}
mat4 :: Type
mat4 = AppN ModuleName.matrix4 "Mat4" []


{-# NOINLINE texture #-}
texture :: Type
texture = AppN ModuleName.webgl "Texture" []



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


nameToFlex :: N.Name -> IO Variable
nameToFlex name =
  UF.fresh $ makeDescriptor $
    maybe FlexVar FlexSuper (toSuper name) (Just name)


nameToRigid :: N.Name -> IO Variable
nameToRigid name =
  UF.fresh $ makeDescriptor $
    maybe RigidVar RigidSuper (toSuper name) name


toSuper :: N.Name -> Maybe SuperType
toSuper name =
  if N.startsWith "number" name then
      Just Number

  else if N.startsWith "comparable" name then
      Just Comparable

  else if N.startsWith "appendable" name then
      Just Appendable

  else if N.startsWith "compappend" name then
      Just CompAppend

  else
      Nothing



-- TO SOURCE TYPES


-- TODO: Attach resulting type to the descriptor so that you
-- never have to do extra work, particularly nice for aliased types
toAnnotation :: Variable -> IO Can.Annotation
toAnnotation variable =
  do  userNames <- getVarNames variable Map.empty
      (tipe, NameState allNames _ _ _ _ _) <-
        State.runStateT (variableToSrcType variable) (makeNameState userNames)
      return $ Can.Forall (Map.map (\_ -> ()) allNames) tipe


toSrcType :: Variable -> IO Can.Type
toSrcType variable =
  do  userNames <- getVarNames variable Map.empty
      State.evalStateT (variableToSrcType variable) (makeNameState userNames)


variableToSrcType :: Variable -> StateT NameState IO Can.Type
variableToSrcType variable =
  do  descriptor <- liftIO $ UF.get variable
      let mark = _mark descriptor
      if mark == occursMark
        then
          return (Can.TVar "∞")

        else
          do  liftIO $ UF.modify variable (\desc -> desc { _mark = occursMark })
              srcType <- contentToSrcType variable (_content descriptor)
              liftIO $ UF.modify variable (\desc -> desc { _mark = mark })
              return srcType


contentToSrcType :: Variable -> Content -> StateT NameState IO Can.Type
contentToSrcType variable content =
  case content of
    Structure term ->
        termToSrcType term

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
        do  srcArgs <- traverse (traverse variableToSrcType) args
            srcType <- variableToSrcType realVariable
            return (Can.TAlias home name srcArgs (Can.Filled srcType))

    Error name ->
        return (Can.TVar name)


termToSrcType :: FlatType -> StateT NameState IO Can.Type
termToSrcType term =
  case term of
    App1 home name args ->
      Can.TType home name <$> traverse variableToSrcType args

    Fun1 a b ->
      Can.TLambda
        <$> variableToSrcType a
        <*> variableToSrcType b

    EmptyRecord1 ->
      return $ Can.TRecord Map.empty Nothing

    Record1 fields extension ->
      do  srcFields <- traverse variableToSrcType fields
          srcExt <- Type.iteratedDealias <$> variableToSrcType extension
          return $
              case srcExt of
                Can.TRecord subFields subExt ->
                    Can.TRecord (Map.union subFields srcFields) subExt

                Can.TVar _ ->
                    Can.TRecord srcFields (Just srcExt)

                _ ->
                    error "Used toSrcType on a type that is not well-formed"

    Unit1 ->
      return Can.TUnit

    Tuple1 a b maybeC ->
      Can.TTuple
        <$> variableToSrcType a
        <*> variableToSrcType b
        <*> traverse variableToSrcType maybeC



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


type TakenNames = Map.Map N.Name Variable


makeNameState :: TakenNames -> NameState
makeNameState taken =
  NameState taken 0 0 0 0 0


getFreshVarName :: (Monad m) => StateT NameState m N.Name
getFreshVarName =
  do  index <- State.gets _normals
      taken <- State.gets _taken
      let (uniqueName, newIndex) = getFreshNormal index taken
      State.modify (\state -> state { _normals = newIndex })
      return uniqueName


getFreshSuperName :: (Monad m) => SuperType -> StateT NameState m N.Name
getFreshSuperName super =
  case super of
    Number ->
      getFreshSuper "number" _numbers (\index state -> state { _numbers = index })

    Comparable ->
      getFreshSuper "comparable" _comparables (\index state -> state { _comparables = index })

    Appendable ->
      getFreshSuper "appendable" _appendables (\index state -> state { _appendables = index })

    CompAppend ->
      getFreshSuper "compappend" _compAppends (\index state -> state { _compAppends = index })


getFreshNormal :: Int -> TakenNames -> (N.Name, Int)
getFreshNormal index taken =
  let
    (postfix, letter) =
      quotRem index 26

    chr = N.fromLetter letter
    name = if postfix <= 0 then chr else N.addIndex chr postfix
  in
    if Map.member name taken then
      getFreshNormal (index + 1) taken

    else
      (name, index + 1)


getFreshSuper
    :: (Monad m)
    => N.Name
    -> (NameState -> Int)
    -> (Int -> NameState -> NameState)
    -> StateT NameState m N.Name
getFreshSuper name getter setter =
  do  index <- State.gets getter
      taken <- State.gets _taken
      let (uniqueName, newIndex) = getFreshSuperHelp name index taken
      State.modify (setter newIndex)
      return uniqueName


getFreshSuperHelp :: N.Name -> Int -> TakenNames -> (N.Name, Int)
getFreshSuperHelp name index taken =
  let
    newName =
      if index <= 0 then name else N.addIndex name index
  in
    if Map.member newName taken then
      getFreshSuperHelp name (index + 1) taken

    else
      (newName, index + 1)



-- GET ALL VARIABLE NAMES


getVarNames :: Variable -> TakenNames -> IO TakenNames
getVarNames var takenNames =
  do  (Descriptor content rank mark copy) <- UF.get var
      if mark == getVarNamesMark
        then return takenNames
        else
        do  UF.set var (Descriptor content rank getVarNamesMark copy)
            case content of
              Error _ ->
                return takenNames

              FlexVar maybeName ->
                case maybeName of
                  Nothing ->
                    return takenNames

                  Just name ->
                    addName 0 name var (FlexVar . Just) takenNames

              FlexSuper super maybeName ->
                case maybeName of
                  Nothing ->
                    return takenNames

                  Just name ->
                    addName 0 name var (FlexSuper super . Just) takenNames

              RigidVar name ->
                addName 0 name var RigidVar takenNames

              RigidSuper super name ->
                addName 0 name var (RigidSuper super) takenNames

              Alias _ _ args _ ->
                foldrM getVarNames takenNames (map snd args)

              Structure flatType ->
                case flatType of
                  App1 _ _ args ->
                    foldrM getVarNames takenNames args

                  Fun1 arg body ->
                    getVarNames arg =<< getVarNames body takenNames

                  EmptyRecord1 ->
                    return takenNames

                  Record1 fields extension ->
                    getVarNames extension =<<
                      foldrM getVarNames takenNames (Map.elems fields)

                  Unit1 ->
                    return takenNames

                  Tuple1 a b Nothing ->
                    getVarNames a =<< getVarNames b takenNames

                  Tuple1 a b (Just c) ->
                    getVarNames a =<< getVarNames b =<< getVarNames c takenNames



-- REGISTER NAME / RENAME DUPLICATES


addName :: Int -> N.Name -> Variable -> (N.Name -> Content) -> TakenNames -> IO TakenNames
addName index givenName var makeContent takenNames =
  let
    indexedName =
      if index <= 0 then
        givenName
      else
        N.addSafeIndex givenName index
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
