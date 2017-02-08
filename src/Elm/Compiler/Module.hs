{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler.Module
    ( Interface, Interfaces
    , ModuleName.Raw
    , nameToPath
    , nameToString, nameFromText
    , hyphenate, dehyphenate
    , RawForJson(RawForJson), fromJson
    , interfaceAliasedTypes, programTypes
    , ModuleName.Canonical(..), qualifiedVar
    )
  where

import qualified Data.Aeson as Json
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
import qualified Generate.JavaScript.Variable as Gen



-- INTERFACES


type Interface = Module.Interface


type Interfaces = Module.Interfaces


interfaceAliasedTypes :: Interface -> Map.Map Text PublicType.Type
interfaceAliasedTypes interface =
    Map.map Extract.extract (Module.iTypes interface)


programTypes :: Interfaces -> ModuleName.Canonical -> Maybe PublicType.Program
programTypes =
  Extract.extractProgram



-- NAMES


newtype RawForJson = RawForJson ModuleName.Raw


fromJson :: RawForJson -> ModuleName.Raw
fromJson (RawForJson raw) =
  raw


qualifiedVar :: ModuleName.Canonical -> Text -> Text
qualifiedVar =
  Gen.qualified



-- STRING CONVERSIONS for RAW NAMES


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



-- JSON for NAME


instance Json.ToJSON RawForJson where
  toJSON (RawForJson name) =
    Json.toJSON (nameToString name)


instance Json.FromJSON RawForJson where
  parseJSON (Json.String text) =
    case nameFromText text of
      Nothing ->
        fail (Text.unpack text ++ " is not a valid module name")

      Just name ->
        return (RawForJson name)

  parseJSON _ =
    fail "expecting the module name to be a string"

