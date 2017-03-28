{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Variable
  ( Raw(..)
  , Canonical(..), Home(..)
  , Global(..)
  , local, topLevel, builtin, fromModule
  , inCore, inHtml, cmd, sub
  , isLocalHome, isCons
  , is, isJson, isMaybe, isArray, isTask, isList
  , isNative, isTuple, isPrimitive, isPrim, isLocal
  , rawToText, toText
  )
  where

import Data.Binary
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Data.Text (Text)

import qualified AST.Helpers as Help
import qualified AST.Module.Name as ModuleName



-- RAW NAMES


newtype Raw = Raw Text
  deriving (Eq, Ord)



-- CANONICAL NAMES


data Home
  = BuiltIn
  | Module !ModuleName.Canonical
  | TopLevel !ModuleName.Canonical
  | Local
  deriving (Eq, Ord)


data Canonical =
  Canonical
    { _home :: !Home
    , _name :: !Text
    }
    deriving (Eq, Ord)



-- GLOBAL NAMES


data Global = Global !ModuleName.Canonical !Text
  deriving (Eq, Ord)



-- HELPERS


local :: Text -> Canonical
local x =
    Canonical Local x


topLevel :: ModuleName.Canonical -> Text -> Canonical
topLevel home x =
    Canonical (TopLevel home) x


builtin :: Text -> Canonical
builtin x =
    Canonical BuiltIn x


fromModule :: ModuleName.Canonical -> Text -> Canonical
fromModule home name =
    Canonical (Module home) name


inCore :: ModuleName.Raw -> Text -> Canonical
inCore home name =
    Canonical (Module (ModuleName.inCore home)) name


inHtml :: ModuleName.Raw -> Text -> Canonical
inHtml home name =
    Canonical (Module (ModuleName.inHtml home)) name


cmd :: Canonical
cmd =
  inCore "Platform.Cmd" "Cmd"


sub :: Canonical
sub =
  inCore "Platform.Sub" "Sub"



-- VARIABLE RECOGNIZERS


isLocalHome :: Home -> Bool
isLocalHome home =
  case home of
    BuiltIn ->
        False

    Module _ ->
        False

    TopLevel _ ->
        True

    Local ->
        True


isCons :: Canonical -> Bool
isCons var =
  case var of
    Canonical BuiltIn "::" ->
      True

    _ ->
      False


is :: ModuleName.Raw -> Text -> Canonical -> Bool
is home name var =
    var == inCore home name


isJson :: Canonical -> Bool
isJson =
    is "Json.Encode" "Value"


isMaybe :: Canonical -> Bool
isMaybe =
    is "Maybe" "Maybe"

isArray :: Canonical -> Bool
isArray =
    is "Array" "Array"


isTask :: Canonical -> Bool
isTask =
    is "Task" "Task"


isList :: Canonical -> Bool
isList v =
    v == Canonical BuiltIn "List"


isNative :: Canonical -> Bool
isNative v =
    case v of
      Canonical (Module name) _ ->
        ModuleName.canonicalIsNative name

      _ ->
        False


isTuple :: Canonical -> Bool
isTuple v =
    case v of
      Canonical BuiltIn name ->
        Help.isTuple name

      _ ->
        False


isPrimitive :: Canonical -> Bool
isPrimitive v =
  case v of
    Canonical BuiltIn name ->
      Set.member name primitiveSet

    _ ->
      False


primitiveSet :: Set.Set Text
primitiveSet =
  Set.fromList ["Int","Float","String","Bool"]


isPrim :: Text -> Canonical -> Bool
isPrim prim (Canonical home name) =
    case home of
      BuiltIn ->
          name == prim

      _ ->
          False


isLocal :: (Text -> Bool) -> Canonical -> Bool
isLocal check (Canonical home name) =
  case home of
    Local ->
        check name

    _ ->
        False



-- VARIABLE TO TEXT


rawToText :: Raw -> Text
rawToText (Raw name) =
  name


toText :: Canonical -> Text
toText (Canonical home name) =
  case home of
    BuiltIn ->
      name

    Module moduleName ->
      ModuleName.canonicalToText moduleName <> "." <> name

    TopLevel _ ->
      name

    Local ->
      name



-- BINARY SERIALIZATION


instance Binary Global where
  put (Global home name) =
    put home >> put name

  get =
    Global <$> get <*> get


instance Binary Canonical where
  put (Canonical home name) =
    case home of
      BuiltIn ->
        putWord8 0 >> put name

      Module path ->
        putWord8 1 >> put path >> put name

      TopLevel path ->
        putWord8 2 >> put path >> put name

      Local ->
        putWord8 3 >> put name

  get =
    do  tag <- getWord8
        case tag of
          0 -> Canonical BuiltIn <$> get
          1 -> Canonical . Module <$> get <*> get
          2 -> Canonical . TopLevel <$> get <*> get
          3 -> Canonical Local <$> get
          _ -> error "Unexpected tag when deserializing canonical variable"
