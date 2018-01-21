{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE BangPatterns, MagicHash, OverloadedStrings, UnboxedTuples #-}
module Json.Decode
  ( parse
  -- re-export from Json.Decode.Internals
  , Json.Decoder, Json.Error(..)
  , Json.string, Json.text, Json.bool, Json.int
  , Json.list, Json.dict, Json.maybe
  , Json.field, Json.at
  , Json.index
  , Json.map, Json.map2
  , Json.succeed, Json.fail
  , Json.andThen, Json.oneOf
  )
  where


import Prelude hiding (length)
import qualified Data.ByteString.Internal as B
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Word (Word8(..))

import qualified Json.Decode.Internals as Json
import qualified Parse.Primitives as P
import qualified Parse.Primitives.Internals as I
import Parse.Primitives.Internals (Parser(..), State(..), noError)
import qualified Parse.Primitives.Keyword as Keyword
import qualified Parse.Primitives.Symbol as Symbol
import qualified Reporting.Error.Syntax as E
import qualified Reporting.Region as R



-- PARSE


data Result e a
  = BadJson E.Error
  | BadContent (Json.Error e)
  | Success a


parse :: Json.Decoder e a -> B.ByteString -> Result e a
parse (Json.Decoder run) bytestring =
  case P.run jsonFile bytestring of
    Left err ->
      BadJson err

    Right value ->
      case run id value of
        Left err ->
          BadContent err

        Right answer ->
          Success answer



-- JSON


jsonFile :: Parser Json.Value
jsonFile =
  do  spaces
      value <- jsonValue
      spaces
      P.endOfFile
      return value


jsonValue :: Parser Json.Value
jsonValue =
  do  start <- P.getPosition
      I.oneOf
        [ Json.String <$> string
        , object start
        , array start
        , number
        , Keyword.jsonTrue >> return Json.TRUE
        , Keyword.jsonFalse >> return Json.FALSE
        , Keyword.jsonNull >> return Json.NULL
        ]



-- OBJECT


object :: R.Position -> Parser Json.Value
object start =
  do  Symbol.leftCurly
      P.inContext start E.ExprRecord $
        do  spaces
            I.oneOf
              [ do  entry <- field
                    spaces
                    objectHelp [entry]
              , do  Symbol.rightCurly
                    return (Json.Object [] HashMap.empty)
              ]


objectHelp :: [(Text.Text, Json.Value)] -> Parser Json.Value
objectHelp revEntries =
  I.oneOf
    [
      do  Symbol.comma
          spaces
          entry <- field
          spaces
          objectHelp (entry:revEntries)
    ,
      do  Symbol.rightCurly
          return (Json.Object (reverse revEntries) (HashMap.fromList revEntries))
    ]


field :: Parser (Text.Text, Json.Value)
field =
  do  key <- string
      spaces
      Symbol.hasType
      spaces
      value <- jsonValue
      return (key, value)



-- ARRAY


array :: R.Position -> Parser Json.Value
array start =
  do  Symbol.leftSquare
      P.inContext start E.ExprList $
        do  spaces
            I.oneOf
              [ do  entry <- jsonValue
                    spaces
                    arrayHelp 1 [entry]
              , do  Symbol.rightSquare
                    return (Json.Array Vector.empty)
              ]


arrayHelp :: Int -> [Json.Value] -> Parser Json.Value
arrayHelp !length revEntries =
  I.oneOf
    [
      do  Symbol.comma
          spaces
          entry <- jsonValue
          spaces
          arrayHelp (length + 1) (entry:revEntries)
    ,
      do  Symbol.rightSquare
          return $ Json.Array $ Vector.reverse $
            Vector.fromListN length revEntries
    ]



-- NUMBER


number :: Parser Json.Value
number =
  error "TODO"



-- STRING


string :: Parser Text.Text
string =
  Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ eerr ->
    if offset < terminal && I.unsafeIndex fp offset == 0x22 {- " -} then

      case stringHelp fp (offset + 1) terminal row (col + 1) of
        Err err ->
          cerr err

        Ok newOffset newRow newCol ->
          let
            !newState = State fp newOffset terminal indent newRow newCol ctx
            !content = Text.decodeUtf8 (B.PS fp offset (newOffset - offset))
          in
            cok content newState noError

    else
      eerr noError


data StringResult
  = Err E.ParseError
  | Ok !Int !Int !Int


stringHelp :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> StringResult
stringHelp fp offset terminal row col =
  if offset >= terminal then
    Err (E.ParseError row col E.EndOfFile_String)

  else
    case I.unsafeIndex fp offset of
      0x22 {- " -} ->
        Ok (offset + 1) row (col + 1)

      0x0A {- \n -} ->
        Err (E.ParseError row col E.NewLineInString)

      0x5C {- \ -} ->
        let
          !offset1 = offset + 1
        in
        if offset1 < terminal then

          let
            !word = I.unsafeIndex fp offset1
            !newOffset = offset1 + I.getCharWidth fp offset1 terminal word
          in
          stringHelp fp newOffset terminal row (col + 2)

        else
          Err (E.ParseError (row + 1) col E.EndOfFile_String)

      word ->
        let !newOffset = offset + I.getCharWidth fp offset terminal word in
        stringHelp fp newOffset terminal row (col + 1)




-- SPACES


spaces :: Parser ()
spaces =
  Parser $ \(State fp offset terminal indent row col ctx) cok _ _ _ ->
    let
      (# newOffset, newRow, newCol #) =
        eatSpaces fp offset terminal row col

      !newState =
        State fp newOffset terminal indent newRow newCol ctx
    in
    cok () newState noError


eatSpaces :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> (# Int, Int, Int #)
eatSpaces fp offset terminal row col =
  if offset >= terminal then
    (# offset, row, col #)

  else
    case I.unsafeIndex fp offset of
      0x20 {-   -}  -> eatSpaces fp (offset + 1) terminal row (col + 1)
      0x09 {- \t -} -> eatSpaces fp (offset + 1) terminal row (col + 1)
      0x0A {- \n -} -> eatSpaces fp (offset + 1) terminal (row + 1) 1
      0x0D {- \r -} -> eatSpaces fp (offset + 1) terminal row col
      _ ->
        (# offset, row, col #)
