{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Json.Encode
  ( write
  , encode
  , Value
  , array
  , object
  , text
  , string
  , bool
  , dict
  , list
  )
  where


import Control.Arrow ((***))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Builder as B
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Numeric
import System.IO (IOMode(WriteMode), withBinaryFile)



-- VALUES


data Value
  = Array [Value]
  | Object [(String, Value)]
  | String B.Builder
  | Boolean Bool


array :: [Value] -> Value
array =
  Array


object :: [(String,Value)] -> Value
object =
  Object


text :: Text.Text -> Value
text txt =
  String (encodeString (Text.unpack txt))


string :: String -> Value
string str =
  String (encodeString str)


bool :: Bool -> Value
bool =
  Boolean


dict :: (k -> String) -> (v -> Value) -> Map.Map k v -> Value
dict encodeKey encodeValue pairs =
  Object $ map (encodeKey *** encodeValue) (Map.toList pairs)


list :: (a -> Value) -> [a] -> Value
list encodeEntry entries =
  Array $ map encodeEntry entries



-- WRITE TO FILE


write :: FilePath -> Value -> IO ()
write path value =
  withBinaryFile path WriteMode $ \handle ->
    B.hPutBuilder handle (encode value)



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


encodeObject :: BSC.ByteString -> (String, Value) -> [(String, Value)] -> B.Builder
encodeObject =
  encodeSequence objectOpen objectClose encodeField


objectOpen :: B.Builder
objectOpen =
  B.string7 "{\n"


objectClose :: B.Builder
objectClose =
  B.char7 '}'


encodeField :: BSC.ByteString -> (String, Value) -> B.Builder
encodeField indent (key, value) =
  encodeString key <> B.string7 ": " <> encodeHelp indent value



-- ENCODE SEQUENCE


encodeSequence :: B.Builder -> B.Builder -> (BSC.ByteString -> a -> B.Builder) -> BSC.ByteString -> a -> [a] -> B.Builder
encodeSequence open close encodeEntry indent first rest =
  let
    newIndent =
      indent <> "\t"

    newIndentBuilder =
      B.byteString newIndent

    closer =
      newline <> B.byteString indent <> close

    addValue field builder =
      builder
      <> commaNewline
      <> newIndentBuilder
      <> encodeEntry newIndent field
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



-- ENCODE STRING


encodeString :: String -> B.Builder
encodeString str =
    B.char7 '"' <> escape str <> B.char7 '"'


escape :: String -> B.Builder
escape str =
  let
    (front, back) =
      break isEscape str
  in
    case back of
      "" ->
        B.stringUtf8 front

      char : rest ->
        B.stringUtf8 front <> escapeChar char <> escape rest


isEscape :: Char -> Bool
isEscape c =
  c == '\"' || c == '\\' || c < '\x20'


escapeChar :: Char -> B.Builder
escapeChar char =
  case char of
    '\"' -> B.string7 "\\\""
    '\\' -> B.string7 "\\\\"
    '\n' -> B.string7 "\\n"
    '\r' -> B.string7 "\\r"
    '\t' -> B.string7 "\\t"
    _    -> if char < '\x20' then toEscapeCode char else B.charUtf8 char


toEscapeCode :: Char -> B.Builder
toEscapeCode char =
  let
    hex =
      Numeric.showHex (fromEnum char) ""

    code =
      "\\u" ++ replicate (4 - length hex) '0' ++ hex
  in
    B.string7 code
