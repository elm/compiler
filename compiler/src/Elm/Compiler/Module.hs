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
import qualified Data.Name as Name
import qualified Data.Text as Text
import qualified System.FilePath as FP

import qualified AST.Module.Name as ModuleName
import qualified Elm.Interface as I
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode



-- NAMES


type Raw = Name.Name


nameToString :: Raw -> String
nameToString =
  Name.toString


nameToSlashPath :: Raw -> FilePath
nameToSlashPath name =
  map (\c -> if c == '.' then FP.pathSeparator else c) (Name.toString name)


nameToHyphenPath :: Raw -> FilePath
nameToHyphenPath name =
  map (\c -> if c == '.' then '-' else c) (Name.toString name)


fromHyphenPath :: Text.Text -> Maybe Raw
fromHyphenPath txt =
  let str = Text.unpack txt in
  if all isGoodChunk (splitOn '-' str)
    then Just (Name.fromString (map (\c -> if c == '-' then '.' else c) str))
    else Nothing



-- JSON


encode :: Raw -> Encode.Value
encode =
  Encode.name


decoder :: Decode.Decoder Text.Text Raw
decoder =
  do  txt <- Decode.text
      let str = Text.unpack txt
      if all isGoodChunk (splitOn '.' str)
        then Decode.succeed (Name.fromString str)
        else Decode.fail txt


isGoodChunk :: String -> Bool
isGoodChunk chunk =
  case chunk of
    [] ->
      False

    first : rest ->
      Char.isUpper first && all isGoodChar rest


isGoodChar :: Char -> Bool
isGoodChar char =
  Char.isAlphaNum char || char == '_'


splitOn :: Char -> String -> [String]
splitOn sep string =
  uncurry (:) (splitOnHelp sep string)


splitOnHelp :: Char -> String -> (String, [String])
splitOnHelp sep string =
  case string of
    [] ->
      ("",[])

    char : rest ->
      let
        (chunk,chunks) = splitOnHelp sep rest
      in
      if char == sep then
        ("", chunk:chunks)
      else
        (char:chunk, chunks)

