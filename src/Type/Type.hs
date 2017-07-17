{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Type
  ( Variable
  , FlatType(..)
  , Type(..)
  , Descriptor(Descriptor)
  , Content(..)
  , Flex(..)
  , Super(..)
  , noRank
  , outermostRank
  , noMark
  , initialMark
  , (==>), float, char, string, bool
  , mkFlexVar
  , mkFlexNumber
  , mkNamedVar
  , toSrcType
  )
  where

import Control.Monad (when)
import Control.Monad.State.Strict (StateT, liftIO)
import qualified Control.Monad.State.Strict as State
import qualified Data.Char as Char
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)

import qualified AST.Type as T
import qualified AST.Variable as Var
import qualified Type.UnionFind as UF



-- TYPE PRIMITIVES


type Variable =
    UF.Point Descriptor


data FlatType
    = App1 Var.Canonical [Variable]
    | Fun1 Variable Variable
    | EmptyRecord1
    | Record1 (Map.Map Text Variable) Variable


data Type
    = PlaceHolder Text
    | AliasN Var.Canonical [(Text, Type)] Type
    | VarN Variable
    | AppN Var.Canonical [Type]
    | FunN Type Type
    | EmptyRecordN
    | RecordN (Map.Map Text Type) Type



-- DESCRIPTORS


data Descriptor =
  Descriptor
    { _content :: Content
    , _rank :: Int
    , _mark :: Int
    , _copy :: Maybe Variable
    }


data Content
    = Structure FlatType
    | Var Flex (Maybe Super) (Maybe Text)
    | Alias Var.Canonical [(Text,Variable)] Variable
    | Error Text


makeDescriptor :: Content -> Descriptor
makeDescriptor content =
  Descriptor content noRank noMark Nothing


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
noRank = 0


outermostRank :: Int
outermostRank = 1


noMark :: Int
noMark = 2


initialMark :: Int
initialMark = 3


occursMark :: Int
occursMark =
  1


getVarNamesMark :: Int
getVarNamesMark =
  0



-- TYPE HELPERS


infixr 9 ==>


{-# INLINE (==>) #-}
(==>) :: Type -> Type -> Type
(==>) =
  FunN


{-# NOINLINE float #-}
float :: Type
float =
  AppN Var.float []


{-# NOINLINE char #-}
char :: Type
char =
  AppN Var.char []


{-# NOINLINE string #-}
string :: Type
string =
  AppN Var.string []


{-# NOINLINE bool #-}
bool :: Type
bool =
  AppN Var.bool []



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
  Var Flex Nothing Nothing



-- MAKE FLEX NUMBERS


mkFlexNumber :: IO Variable
mkFlexNumber =
  UF.fresh flexNumberDescriptor


{-# NOINLINE flexNumberDescriptor #-}
flexNumberDescriptor :: Descriptor
flexNumberDescriptor =
  makeDescriptor (unnamedFlexSuper Number)


unnamedFlexSuper :: Super -> Content
unnamedFlexSuper super =
  Var Flex (Just super) Nothing



-- MAKE NAMED FLEX VARIABLES


mkNamedVar :: Flex -> Text -> IO Variable
mkNamedVar flex name =
    UF.fresh $ makeDescriptor $ Var flex (toSuper name) (Just name)


toSuper :: Text -> Maybe Super
toSuper name =
  if Text.isPrefixOf "number" name then
      Just Number

  else if Text.isPrefixOf "comparable" name then
      Just Comparable

  else if Text.isPrefixOf "appendable" name then
      Just Appendable

  else if Text.isPrefixOf "compappend" name then
      Just CompAppend

  else
      Nothing



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

    Error name ->
        return (T.Var name)


termToSrcType :: FlatType -> StateT NameState IO T.Canonical
termToSrcType term =
  case term of
    App1 name args ->
      T.Type name <$> traverse variableToSrcType args

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


getFreshName :: (Monad m) => Maybe Super -> StateT NameState m Text
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
  do  desc <- liftIO $ UF.descriptor var
      case _mark desc == getVarNamesMark of
        True ->
          return ()

        False ->
          do  liftIO $ UF.setDescriptor var (desc { _mark = getVarNamesMark })
              case _content desc of
                Error _ ->
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

                Structure (App1 _ args) ->
                  mapM_ getVarNames args

                Structure (Fun1 arg body) ->
                  do  getVarNames arg
                      getVarNames body

                Structure EmptyRecord1 ->
                  return ()

                Structure (Record1 fields extension) ->
                  do  mapM_ getVarNames fields
                      getVarNames extension


addVarName :: Int -> Text -> Variable -> Flex -> Maybe Super -> TakenNames -> IO TakenNames
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


makeIndexedName :: Text -> Int -> Text
makeIndexedName name index =
  if index <= 0 then
    name

  else if Char.isDigit (Text.last name) then
    name <> Text.pack ('_' : show index)

  else
    name <> Text.pack (show index)
