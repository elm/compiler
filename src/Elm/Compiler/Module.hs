{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler.Module
  -- interfaces
  ( I.Interface
  , I.Interfaces
  , interfaceAliasedTypes

  -- module names
  , Raw
  , nameToPath
  , nameToString
  , nameFromText
  , hyphenate
  , dehyphenate
  , encode
  , decoder

  -- canonical names
  , ModuleName.Canonical(..)
  )
  where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import System.FilePath ((</>))

import qualified AST.Module.Name as ModuleName
import qualified Elm.Compiler.Type as Type
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Elm.Interface as I
import qualified Elm.Name as N
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode



-- INTERFACES


interfaceAliasedTypes :: I.Interface -> Map.Map N.Name Type.Type
interfaceAliasedTypes interface =
  Map.map Extract.fromAnnotation (I._types interface)



-- NAMES


type Raw = N.Name


nameToPath :: Raw -> FilePath
nameToPath name =
  List.foldl1 (</>) (map Text.unpack (Text.splitOn "." name))


nameToString :: Raw -> String
nameToString name =
  Text.unpack name


nameFromText :: Text.Text -> Maybe Raw
nameFromText =
  fromText '.'


hyphenate :: Raw -> Text.Text
hyphenate name =
  Text.replace "." "-" name


dehyphenate :: Text.Text -> Maybe Raw
dehyphenate =
  fromText '-'


fromText :: Char -> Text.Text -> Maybe Raw
fromText sep name =
  let
    chunks =
      Text.splitOn (Text.singleton sep) name
  in
    if all isGoodChunk chunks then Just name else Nothing


isGoodChunk :: Text.Text -> Bool
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


encode :: Raw -> Encode.Value
encode =
  Encode.text


decoder :: Decode.Decoder Raw
decoder =
  do  txt <- Decode.text
      case nameFromText txt of
        Nothing ->
          Decode.fail "Expecting a module name like \"Html.Events\""

        Just name ->
          Decode.succeed name
