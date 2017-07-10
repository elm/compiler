{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler.Module
  -- interfaces
  ( Module.Interface
  , Module.Interfaces
  , Module.privatize
  , interfaceAliasedTypes
  , programTypes

  -- module names
  , ModuleName.Raw
  , nameToPath
  , nameToString
  , nameFromText
  , hyphenate
  , dehyphenate
  , encode
  , decoder

  -- canonical names
  , ModuleName.Canonical(..)
  , canonicalToMain
  )
  where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Text (Text)
import System.FilePath ((</>))

import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified Elm.Compiler.Type as PublicType
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Generate.JavaScript.Variable as JS
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode



-- INTERFACES


interfaceAliasedTypes :: Module.Interface -> Map.Map Text PublicType.Type
interfaceAliasedTypes interface =
  Map.map Extract.extract (Module.iTypes interface)


programTypes :: Module.Interfaces -> ModuleName.Canonical -> Maybe PublicType.Program
programTypes =
  Extract.extractProgram



-- NAMES


nameToPath :: ModuleName.Raw -> FilePath
nameToPath name =
  List.foldl1 (</>) (map Text.unpack (Text.splitOn "." name))


nameToString :: ModuleName.Raw -> String
nameToString name =
  Text.unpack name


nameFromText :: Text -> Maybe ModuleName.Raw
nameFromText =
  fromText '.'


hyphenate :: ModuleName.Raw -> Text
hyphenate name =
  Text.replace "." "-" name


dehyphenate :: Text -> Maybe ModuleName.Raw
dehyphenate =
  fromText '-'


fromText :: Char -> Text -> Maybe ModuleName.Raw
fromText sep name =
  let
    chunks =
      Text.splitOn (Text.singleton sep) name
  in
    if all isGoodChunk chunks then Just name else Nothing


isGoodChunk :: Text -> Bool
isGoodChunk chunk =
  case Text.uncons chunk of
    Nothing ->
      False

    Just (first, rest) ->
      Char.isUpper first && Text.all isGoodChar rest


isGoodChar :: Char -> Bool
isGoodChar char =
  Char.isAlphaNum char || char == '_'



-- JSON


encode :: ModuleName.Raw -> Encode.Value
encode =
  Encode.text


decoder :: Decode.Decoder ModuleName.Raw
decoder =
  do  txt <- Decode.text
      case nameFromText txt of
        Nothing ->
          Decode.fail "Expecting a module name like \"Html.Events\""

        Just name ->
          Decode.succeed name



-- CANONICAL TO MAIN


canonicalToMain :: ModuleName.Canonical -> Text
canonicalToMain home =
  JS.globalToName home "main"
