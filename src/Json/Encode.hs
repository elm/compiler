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
  , text
  , name
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
import qualified Data.ByteString.Builder.Prim as BP
import Data.ByteString.Builder.Prim ((>*<))
import qualified Data.Map as Map
import qualified Data.Scientific as Sci
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Word as W
import System.IO (IOMode(WriteMode), withBinaryFile)

import qualified Elm.Name as N



-- VALUES


data Value
  = Array [Value]
  | Object [(Text.Text, Value)]
  | String B.Builder
  | Boolean Bool
  | Integer Int
  | Number Sci.Scientific
  | Null


array :: [Value] -> Value
array =
  Array


object :: [(Text.Text,Value)] -> Value
object =
  Object


text :: Text.Text -> Value
text txt =
  String (encodeText txt)


name :: N.Name -> Value
name nm =
  String ("\"" <> N.toBuilder nm <> "\"")


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


dict :: (k -> Text.Text) -> (v -> Value) -> Map.Map k v -> Value
dict encodeKey encodeValue pairs =
  Object $ map (encodeKey *** encodeValue) (Map.toList pairs)


list :: (a -> Value) -> [a] -> Value
list encodeEntry entries =
  Array $ map encodeEntry entries



-- HELPERS


(==>) :: a -> b -> (a, b)
(==>) a b =
  (a, b)



-- WRITE TO FILE


write :: FilePath -> Value -> IO ()
write path value =
  withBinaryFile path WriteMode $ \handle ->
    B.hPutBuilder handle (encode value)


writeUgly :: FilePath -> Value -> IO ()
writeUgly path value =
  withBinaryFile path WriteMode $ \handle ->
    B.hPutBuilder handle (encodeUgly value)



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
          B.char7 char <> encodeText key <> B.char7 ':' <> encodeUgly entry
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


encodeObject :: BSC.ByteString -> (Text.Text, Value) -> [(Text.Text, Value)] -> B.Builder
encodeObject =
  encodeSequence objectOpen objectClose encodeField


objectOpen :: B.Builder
objectOpen =
  B.string7 "{\n"


objectClose :: B.Builder
objectClose =
  B.char7 '}'


encodeField :: BSC.ByteString -> (Text.Text, Value) -> B.Builder
encodeField indent (key, value) =
  encodeText key <> B.string7 ": " <> encodeHelp indent value



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



-- ENCODE TEXT


encodeText :: Text.Text -> B.Builder
encodeText txt =
  B.char7 '"' <> Text.encodeUtf8BuilderEscaped escapeAscii txt <> B.char7 '"'


{-# INLINE escapeAscii #-}
escapeAscii :: BP.BoundedPrim W.Word8
escapeAscii =
    BP.condB (== 0x5C {- \\ -}) (ascii2 ('\\','\\')) $
    BP.condB (== 0x22 {- \" -}) (ascii2 ('\\','"' )) $
    BP.condB (>= 0x20         ) (BP.liftFixedToBounded BP.word8) $
    BP.condB (== 0x0A {- \n -}) (ascii2 ('\\','n' )) $
    BP.condB (== 0x0D {- \r -}) (ascii2 ('\\','r' )) $
    BP.condB (== 0x09 {- \t -}) (ascii2 ('\\','t' )) $
    BP.liftFixedToBounded hexEscape -- fallback for chars < 0x20
  where
    hexEscape :: BP.FixedPrim W.Word8
    hexEscape = (\c -> ('\\', ('u', fromIntegral c))) BP.>$<
        BP.char8 >*< BP.char8 >*< BP.word16HexFixed


{-# INLINE ascii2 #-}
ascii2 :: (Char, Char) -> BP.BoundedPrim a
ascii2 cs =
  BP.liftFixedToBounded $ const cs BP.>$< BP.char7 >*< BP.char7
