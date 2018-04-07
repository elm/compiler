{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE BangPatterns, MagicHash, OverloadedStrings, UnboxedTuples #-}
module Json.Decode
  ( parse
  -- re-export from Json.Decode.Internals
  , Json.Decoder
  , Json.string, Json.text, Json.name, Json.bool, Json.int
  , Json.list, Json.dict, Json.pairs, Json.maybe
  , Json.field, Json.at
  , Json.index
  , Json.map, Json.map2, Json.mapError
  , Json.succeed, Json.fail
  , Json.andThen, Json.oneOf
  )
  where


import Prelude hiding (length)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Internal as B
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Word (Word8(..))

import qualified Json.Decode.Error as DecodeError
import qualified Json.Decode.Internals as Json
import qualified Parse.Primitives as P
import qualified Parse.Primitives.Internals as I
import Parse.Primitives.Internals (Parser(..), State(..), noError)
import qualified Parse.Primitives.Keyword as Keyword
import qualified Parse.Primitives.Number as Number
import qualified Parse.Primitives.Symbol as Symbol
import qualified Reporting.Doc as D
import qualified Reporting.Error.Syntax as E
import qualified Reporting.Region as R
import qualified Reporting.Render.Code as Code



-- PARSE


parse :: String -> (e -> [D.Doc]) -> Json.Decoder e a -> B.ByteString -> Either D.Doc a
parse rootName userErrorToDocs (Json.Decoder run) bytestring =
  let
    source =
      Code.toSource (Text.replace "\t" " " (Text.decodeUtf8 bytestring))

    toDoc err =
      DecodeError.toDoc rootName source userErrorToDocs err
  in
  case P.run jsonFile bytestring of
    Left err ->
      Left (toDoc (DecodeError.BadJson err))

    Right value ->
      case run value of
        Left err ->
          Left (toDoc (DecodeError.BadContent err))

        Right answer ->
          Right answer



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
  Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ eerr ->
    if offset < terminal then

      let
        !word = I.unsafeIndex fp offset
        !offset1 = offset + 1
      in
      if word <= 0x39 {- 9 -} && word >= 0x31 {- 1 -} then
        case Number.chompInt fp offset1 terminal (fromIntegral (word - 0x30 {- 0 -})) of
          Number.Err newOffset problem ->
            cerr (E.ParseError row (col + (newOffset - offset)) problem)

          Number.OkInt newOffset n ->
            let
              !newState = State fp newOffset terminal indent row (col + (newOffset - offset)) ctx
            in
            cok (Json.Integer n) newState noError

          Number.OkFloat newOffset ->
            let
              !length = newOffset - offset
              !scientific = read $ Char8.unpack $ B.PS fp offset length
              !newState = State fp newOffset terminal indent row (col + length) ctx
            in
            cok (Json.Float scientific) newState noError

      else if word == 0x30 {- 0 -} then
        cok (Json.Integer 0) (State fp offset1 terminal indent row (col + 1) ctx) noError

      else
        eerr noError

    else
      eerr noError




-- STRING


string :: Parser Text.Text
string =
  Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ eerr ->
    if offset < terminal && I.unsafeIndex fp offset == 0x22 {- " -} then

      let
        !offset1 = offset + 1
      in
      case stringHelp fp offset1 terminal row (col + 1) of
        Err err ->
          cerr err

        Ok newOffset newRow newCol ->
          let
            !newState = State fp newOffset terminal indent newRow newCol ctx
            !content = Text.decodeUtf8 (B.PS fp offset1 (newOffset - offset1 - 1))
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
