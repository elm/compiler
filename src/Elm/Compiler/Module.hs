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
  , RawForJson(..)
  , fromJson

  -- canonical names
  , ModuleName.Canonical(..)
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



-- JSON for NAME


newtype RawForJson =
  RawForJson ModuleName.Raw


fromJson :: RawForJson -> ModuleName.Raw
fromJson (RawForJson raw) =
  raw


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

