{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Json.Encode
  ( write
  , encode
  , writeUgly
  , encodeUgly
  , Value(..)
  , array
  , object
  , string
  , name
  , chars
  , bool
  , int
  , number
  , null
  , dict
  , list
  , (==>)
  )
  where


import Prelude hiding (null)
import Control.Arrow ((***))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Builder as B
import qualified Data.Map as Map
import qualified Data.Scientific as Sci
import qualified Data.Name as Name
import qualified Data.Utf8 as Utf8

import qualified File
import qualified Json.String as Json



-- VALUES


data Value
  = Array [Value]
  | Object [(Json.String, Value)]
  | String B.Builder
  | Boolean Bool
  | Integer Int
  | Number Sci.Scientific
  | Null


array :: [Value] -> Value
array =
  Array


object :: [(Json.String, Value)] -> Value
object =
  Object


string :: Json.String -> Value
string str =
  String (B.char7 '"' <> Json.toBuilder str <> B.char7 '"')


name :: Name.Name -> Value
name nm =
  String (B.char7 '"' <> Name.toBuilder nm <> B.char7 '"')


bool :: Bool -> Value
bool =
  Boolean


int :: Int -> Value
int =
  Integer


number :: Sci.Scientific -> Value
number =
  Number


null :: Value
null =
  Null


dict :: (k -> Json.String) -> (v -> Value) -> Map.Map k v -> Value
dict encodeKey encodeValue pairs =
  Object $ map (encodeKey *** encodeValue) (Map.toList pairs)


list :: (a -> Value) -> [a] -> Value
list encodeEntry entries =
  Array $ map encodeEntry entries



-- CHARS


chars :: [Char] -> Value -- PERF can this be done better? Look for examples.
chars chrs =
  String (B.char7 '"' <> B.stringUtf8 (escape chrs) <> B.char7 '"')


escape :: [Char] -> [Char]
escape chrs =
  case chrs of
    [] ->
      []

    c:cs
      | c == '\r' -> '\\' : 'r'  : escape cs
      | c == '\n' -> '\\' : 'n'  : escape cs
      | c == '\"' -> '\\' : '"'  : escape cs
      | c == '\\' -> '\\' : '\\' : escape cs
      | otherwise -> c : escape cs



-- HELPERS


(==>) :: [Char] -> value -> (Json.String, value)
(==>) key value =
  (Json.fromChars key, value)



-- WRITE TO FILE


write :: FilePath -> Value -> IO ()
write path value =
  File.writeBuilder path (encode value <> "\n")


writeUgly :: FilePath -> Value -> IO ()
writeUgly path value =
  File.writeBuilder path (encodeUgly value)



-- ENCODE UGLY


encodeUgly :: Value -> B.Builder
encodeUgly value =
  case value of
    Array [] ->
      B.string7 "[]"

    Array (first : rest) ->
      let
        encodeEntry entry =
          B.char7 ',' <> encodeUgly entry
      in
        B.char7 '[' <> encodeUgly first <> mconcat (map encodeEntry rest) <> B.char7 ']'

    Object [] ->
      B.string7 "{}"

    Object (first : rest) ->
      let
        encodeEntry char (key, entry) =
          B.char7 char <> B.char7 '"' <> Utf8.toBuilder key <> B.string7 "\":" <> encodeUgly entry
      in
        encodeEntry '{' first <> mconcat (map (encodeEntry ',') rest) <> B.char7 '}'

    String builder ->
      builder

    Boolean boolean ->
      B.string7 (if boolean then "true" else "false")

    Integer n ->
      B.intDec n

    Number scientific ->
      B.string7 (Sci.formatScientific Sci.Generic Nothing scientific)

    Null ->
      "null"



-- ENCODE


encode :: Value -> B.Builder
encode value =
  encodeHelp "" value


encodeHelp :: BSC.ByteString -> Value -> B.Builder
encodeHelp indent value =
  case value of
    Array [] ->
      B.string7 "[]"

    Array (first : rest) ->
      encodeArray indent first rest

    Object [] ->
      B.string7 "{}"

    Object (first : rest) ->
      encodeObject indent first rest

    String builder ->
      builder

    Boolean boolean ->
      B.string7 (if boolean then "true" else "false")

    Integer n ->
      B.intDec n

    Number scientific ->
      B.string7 (Sci.formatScientific Sci.Generic Nothing scientific)

    Null ->
      "null"



-- ENCODE ARRAY


encodeArray :: BSC.ByteString -> Value -> [Value] -> B.Builder
encodeArray =
  encodeSequence arrayOpen arrayClose encodeHelp


arrayOpen :: B.Builder
arrayOpen =
  B.string7 "[\n"


arrayClose :: B.Builder
arrayClose =
  B.char7 ']'



-- ENCODE OBJECT


encodeObject :: BSC.ByteString -> (Json.String, Value) -> [(Json.String, Value)] -> B.Builder
encodeObject =
  encodeSequence objectOpen objectClose encodeField


objectOpen :: B.Builder
objectOpen =
  B.string7 "{\n"


objectClose :: B.Builder
objectClose =
  B.char7 '}'


encodeField :: BSC.ByteString -> (Json.String, Value) -> B.Builder
encodeField indent (key, value) =
  B.char7 '"' <> Utf8.toBuilder key <> B.string7 "\": " <> encodeHelp indent value



-- ENCODE SEQUENCE


encodeSequence :: B.Builder -> B.Builder -> (BSC.ByteString -> a -> B.Builder) -> BSC.ByteString -> a -> [a] -> B.Builder
encodeSequence open close encodeEntry indent first rest =
  let
    newIndent =
      indent <> "    "

    newIndentBuilder =
      B.byteString newIndent

    closer =
      newline <> B.byteString indent <> close

    addValue field builder =
      commaNewline
      <> newIndentBuilder
      <> encodeEntry newIndent field
      <> builder
  in
    open
    <> newIndentBuilder
    <> encodeEntry newIndent first
    <> foldr addValue closer rest


commaNewline :: B.Builder
commaNewline =
  B.string7 ",\n"


newline :: B.Builder
newline =
  B.char7 '\n'
