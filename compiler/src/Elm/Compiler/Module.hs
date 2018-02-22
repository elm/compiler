{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler.Module
  -- interfaces
  ( I.Interface
  , I.Interfaces

  -- module names
  , Raw
  , nameToString
  , nameToSlashPath
  , nameToHyphenPath
  , fromHyphenPath
  , encode
  , decoder

  -- canonical names
  , ModuleName.Canonical(..)
  )
  where


import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import System.FilePath ((</>))

import qualified AST.Module.Name as ModuleName
import qualified Elm.Interface as I
import qualified Elm.Name as N
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode



-- NAMES


type Raw = N.Name


nameToString :: Raw -> String
nameToString =
  N.toString


nameToSlashPath :: Raw -> FilePath
nameToSlashPath name =
  List.foldl1 (</>) (map Text.unpack (Text.splitOn "." (N.toText name)))


nameToHyphenPath :: Raw -> FilePath
nameToHyphenPath name =
  Text.unpack (Text.replace "." "-" (N.toText name))


fromHyphenPath :: Text.Text -> Maybe Raw
fromHyphenPath txt =
  if all isGoodChunk (Text.splitOn "-" txt)
    then Just (N.fromText (Text.replace "-" "." txt))
    else Nothing



-- JSON


encode :: Raw -> Encode.Value
encode =
  Encode.name


decoder :: Decode.Decoder Text.Text Raw
decoder =
  do  txt <- Decode.text
      let chunks = Text.splitOn "." txt
      if all isGoodChunk chunks
        then Decode.succeed (N.fromText txt)
        else Decode.fail txt


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


