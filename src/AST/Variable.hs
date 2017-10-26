{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Variable
  ( Canonical(..), Home(..)
  , Global(..)
  , local, topLevel, fromModule
  , inCore, inHtml
  , int, float, char, string
  , list, cons, nil
  , bool, true, false
  , never, task, router, shader, decoder
  , isLocalHome, isCons
  , is, isJson, isMaybe, isArray, isTask, isList
  , isKernel, isPrim, isLocal
  , toText
  )
  where

import Data.Binary
import Data.Monoid ((<>))
import Data.Text (Text)

import qualified AST.Module.Name as ModuleName
import qualified Elm.Package as Pkg



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


fromModule :: ModuleName.Canonical -> Text -> Canonical
fromModule home name =
    Canonical (Module home) name


inCore :: ModuleName.Raw -> Text -> Canonical
inCore home name =
    Canonical (Module (ModuleName.inCore home)) name


inHtml :: ModuleName.Raw -> Text -> Canonical
inHtml home name =
    Canonical (Module (ModuleName.inHtml home)) name



-- BUILT IN TYPES


int :: Canonical
int =
  Canonical BuiltIn "Int"


float :: Canonical
float =
  Canonical BuiltIn "Float"


char :: Canonical
char =
  Canonical BuiltIn "Char"


string :: Canonical
string =
  Canonical BuiltIn "String"



-- LIST


list :: Canonical
list =
  Canonical BuiltIn "List"


cons :: Canonical
cons =
  Canonical BuiltIn "::"


nil :: Canonical
nil =
  Canonical BuiltIn "[]"



-- BOOLEANS


bool :: Canonical
bool =
  Canonical BuiltIn "Bool"


true :: Canonical
true =
  Canonical BuiltIn "True"


false :: Canonical
false =
  Canonical BuiltIn "False"



-- CORE TYPES


never :: Canonical
never =
  inCore "Basics" "Never"


task :: Canonical
task =
  inCore "Platform" "Task"


router :: Canonical
router =
  inCore "Platform" "Router"


shader :: Canonical
shader =
  fromModule (ModuleName.Canonical Pkg.webgl "WebGL") "Shader"


decoder :: Canonical
decoder =
  inCore "Json.Decode" "Decoder"



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
isList var =
    var == Canonical BuiltIn "List"


isKernel :: Canonical -> Bool
isKernel var =
    case var of
      Canonical (Module name) _ ->
        ModuleName.canonicalIsKernel name

      _ ->
        False


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
