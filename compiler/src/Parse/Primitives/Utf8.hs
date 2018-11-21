{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE BangPatterns, MagicHash, OverloadedStrings, UnboxedTuples #-}
module Parse.Primitives.Utf8
  ( string
  , character
  )
  where


import Prelude hiding (length)
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.Utf8 as Utf8
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Word (Word8(..))

import Parse.Primitives.Internals (Parser(..), State(..), noError)
import qualified Parse.Primitives.Internals as I
import qualified Parse.Primitives.Number as Number
import qualified Reporting.Error.Syntax as E



-- CHARACTER


character :: Parser Utf8.Utf8
character =
  Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ eerr ->
    if offset >= terminal || I.unsafeIndex fp offset /= 0x27 {- ' -} then
      eerr noError

    else
      case chompChar fp (offset + 1) terminal (col + 1) 0 placeholder of
        Bad newCol problem ->
          cerr (E.ParseError row newCol problem)

        Good newOffset newCol numChars mostRecent ->
          if numChars /= 1 then
            cerr (E.ParseError row col (E.BadChar newCol))
          else
            let !newState = State fp newOffset terminal indent row newCol ctx in
            cok (Utf8.fromChunks fp [mostRecent]) newState noError


data CharResult
  = Bad Int E.Problem
  | Good Int Int Int Utf8.Chunk


chompChar :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Utf8.Chunk -> CharResult
chompChar fp offset terminal col numChars mostRecent =
  if offset >= terminal then
    Bad col E.EndOfFile_Char

  else
    let
      !word = I.unsafeIndex fp offset
    in
      if word == 0x27 {- ' -} then
        Good (offset + 1) (col + 1) numChars mostRecent

      else if word == 0x0A {- \n -} then
        Bad col E.NewLineInChar

      else if word == 0x22 {- " -} then
        chompChar fp (offset + 1) terminal (col + 1) (numChars + 1) doubleQuote

      else if word == 0x5C {- \ -} then
        case eatEscape fp (offset + 1) terminal of
          EscapeNormal ->
            chompChar fp (offset + 2) terminal (col + 2) (numChars + 1) (Utf8.Slice offset 2)

          EscapeUnicode delta code ->
            chompChar fp (offset + delta) terminal (col + delta) (numChars + 1) (Utf8.CodePoint code)

          EscapeProblem newOffset problem ->
            Bad col (E.BadEscape (newOffset - offset) problem)

          EscapeEndOfFile ->
            Bad col E.EndOfFile_Char

      else
        let !width = I.getCharWidth fp offset terminal word in
        chompChar fp (offset + width) terminal (col + 1) (numChars + 1) (Utf8.Slice offset width)



-- STRINGS


string :: Parser Utf8.Utf8
string =
  Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ eerr ->
    if isDoubleQuote fp offset terminal then

      let
        !offset1 = offset + 1
        result =
          if isDoubleQuote fp offset1 terminal && isDoubleQuote fp (offset + 2) terminal then
            multiString fp (offset + 3) terminal row (col + 3) (offset + 3) mempty
          else
            singleString fp offset1 terminal row (col + 1) offset1 mempty
      in
        case result of
          Err err ->
            cerr err

          Ok newOffset newRow newCol utf8 ->
            let
              !newState = State fp newOffset terminal indent newRow newCol ctx
            in
            cok utf8 newState noError

    else
      eerr noError


{-# INLINE isDoubleQuote #-}
isDoubleQuote :: ForeignPtr Word8 -> Int -> Int -> Bool
isDoubleQuote fp offset terminal =
  offset < terminal && I.unsafeIndex fp offset == 0x22 {- " -}


data StringResult
  = Err E.ParseError
  | Ok !Int !Int !Int !Utf8.Utf8


finalize :: ForeignPtr Word8 -> Int -> Int -> [Utf8.Chunk] -> Utf8.Utf8
finalize fp start end revChunks =
  Utf8.fromChunks fp $ reverse $
    if start == end then
      revChunks
    else
      Utf8.Slice start (end - start) : revChunks


addEscape :: Utf8.Chunk -> Int -> Int -> [Utf8.Chunk] -> [Utf8.Chunk]
addEscape chunk start end revChunks =
  if start == end then
    chunk : revChunks
  else
    chunk : Utf8.Slice start (end - start) : revChunks



-- SINGLE STRINGS


singleString :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Int -> [Utf8.Chunk] -> StringResult
singleString fp offset terminal row col initialOffset revChunks =
  if offset >= terminal then
    Err (E.ParseError row col E.EndOfFile_String)

  else
    let
      !word = I.unsafeIndex fp offset
    in
      if word == 0x22 {- " -} then
        Ok (offset + 1) row (col + 1) $
          finalize fp initialOffset offset revChunks

      else if word == 0x0A {- \n -} then
        Err (E.ParseError row col E.NewLineInString)

      else if word == 0x27 {- ' -} then
        let !newOffset = offset + 1 in
        singleString fp newOffset terminal row (col + 1) newOffset $
          addEscape singleQuote initialOffset offset revChunks

      else if word == 0x5C {- \ -} then
        case eatEscape fp (offset + 1) terminal of
          EscapeNormal ->
            singleString fp (offset + 2) terminal row (col + 2) initialOffset revChunks

          EscapeUnicode delta code ->
            let !newOffset = offset + delta in
            singleString fp newOffset terminal row (col + delta) newOffset $
              addEscape (Utf8.CodePoint code) initialOffset offset revChunks

          EscapeProblem newOffset problem ->
            Err (E.ParseError row col (E.BadEscape (newOffset - offset) problem))

          EscapeEndOfFile ->
            Err (E.ParseError row (col + 1) E.EndOfFile_String)

      else
        let !newOffset = offset + I.getCharWidth fp offset terminal word in
        singleString fp newOffset terminal row (col + 1) initialOffset revChunks



-- MULTI STRINGS


multiString :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Int -> [Utf8.Chunk] -> StringResult
multiString fp offset terminal row col initialOffset revChunks =
  if offset >= terminal then
    Err (E.ParseError row col E.EndOfFile_MultiString)

  else
    let !word = I.unsafeIndex fp offset in
    if word == 0x22 {- " -} && isDoubleQuote fp (offset + 1) terminal && isDoubleQuote fp (offset + 2) terminal then
      Ok (offset + 3) row (col + 3) $
        finalize fp initialOffset offset revChunks

    else if word == 0x27 {- ' -} then
      let !offset1 = offset + 1 in
      multiString fp offset1 terminal row (col + 1) offset1 $
        addEscape singleQuote initialOffset offset revChunks

    else if word == 0x0A {- \n -} then
      let !offset1 = offset + 1 in
      multiString fp offset1 terminal (row + 1) 1 offset1 $
        addEscape newline initialOffset offset revChunks

    else if word == 0x0D {- \r -} then
      let !offset1 = offset + 1 in
      multiString fp offset1 terminal row col offset1 $
        addEscape carriageReturn initialOffset offset revChunks

    else if word == 0x5C {- \ -} then
      case eatEscape fp (offset + 1) terminal of
        EscapeNormal ->
          multiString fp (offset + 2) terminal row (col + 2) initialOffset revChunks

        EscapeUnicode delta code ->
          let !newOffset = offset + delta in
          multiString fp newOffset terminal row (col + delta) newOffset $
            addEscape (Utf8.CodePoint code) initialOffset offset revChunks

        EscapeProblem newOffset problem ->
          Err (E.ParseError row col (E.BadEscape (newOffset - offset) problem))

        EscapeEndOfFile ->
          Err (E.ParseError row (col + 1) E.EndOfFile_MultiString)

    else
      let !newOffset = offset + I.getCharWidth fp offset terminal word in
      multiString fp newOffset terminal row (col + 1) initialOffset revChunks



-- ESCAPE CHARACTERS


data Escape
  = EscapeNormal
  | EscapeUnicode !Int !Int
  | EscapeProblem !Int E.EscapeProblem
  | EscapeEndOfFile


eatEscape :: ForeignPtr Word8 -> Int -> Int -> Escape
eatEscape fp offset terminal =
  if offset >= terminal then
    EscapeEndOfFile

  else
    case I.unsafeIndex fp offset of
      0x6E {- n -} -> EscapeNormal
      0x72 {- r -} -> EscapeNormal
      0x74 {- t -} -> EscapeNormal
      0x22 {- " -} -> EscapeNormal
      0x27 {- ' -} -> EscapeNormal
      0x5C {- \ -} -> EscapeNormal
      0x75 {- u -} -> eatUnicode fp (offset + 1) terminal
      _            -> EscapeProblem offset E.UnknownEscape


eatUnicode :: ForeignPtr Word8 -> Int -> Int -> Escape
eatUnicode fp offset terminal =
  if offset >= terminal || I.unsafeIndex fp offset /= 0x7B {- { -} then
    EscapeProblem offset E.UnicodeSyntax
  else
    let
      !digitOffset = offset + 1
      (# newOffset, code #) = Number.chompHex fp digitOffset terminal
      !numDigits = newOffset - digitOffset
    in
    if newOffset >= terminal || I.unsafeIndex fp newOffset /= 0x7D {- } -} then
      EscapeProblem newOffset E.UnicodeSyntax

    else if code < 0 || 0x10FFFF < code then
      EscapeProblem (newOffset + 1) E.UnicodeRange

    else if numDigits < 4 || 6 < numDigits then
      EscapeProblem (newOffset + 1) $
        E.UnicodeLength numDigits (BS_UTF8.toString (B.PS fp digitOffset numDigits))

    else
      EscapeUnicode (numDigits + 4) code


{-# NOINLINE singleQuote #-}
singleQuote :: Utf8.Chunk
singleQuote =
  Utf8.Escape 0x5C27 {- \' -}


{-# NOINLINE doubleQuote #-}
doubleQuote :: Utf8.Chunk
doubleQuote =
  Utf8.Escape 0x5C22 {- \" -}


{-# NOINLINE newline #-}
newline :: Utf8.Chunk
newline =
  Utf8.Escape 0x5C6E {- \n -}


{-# NOINLINE carriageReturn #-}
carriageReturn :: Utf8.Chunk
carriageReturn =
  Utf8.Escape 0x5C72 {- \r -}


{-# NOINLINE placeholder #-}
placeholder :: Utf8.Chunk
placeholder =
  Utf8.CodePoint 0xFFFD {- replacement character -}
