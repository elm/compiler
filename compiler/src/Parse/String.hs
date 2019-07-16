{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE BangPatterns, MagicHash, OverloadedStrings, UnboxedTuples #-}
module Parse.String
  ( string
  , character
  )
  where


import qualified Data.Utf8 as Utf8
import Data.Word (Word8, Word16)
import Foreign.Ptr (Ptr, plusPtr, minusPtr)

import qualified Elm.String as ES
import Parse.Primitives (Parser, Row, Col)
import qualified Parse.Number as Number
import qualified Parse.Primitives as P
import qualified Reporting.Error.Syntax as E



-- CHARACTER


character :: (Row -> Col -> x) -> (E.Char -> Row -> Col -> x) -> Parser x ES.String
character toExpectation toError =
  P.Parser $ \(P.State src pos end indent row col) cok _ cerr eerr ->
    if pos >= end || P.unsafeIndex pos /= 0x27 {- ' -} then
      eerr row col toExpectation

    else
      case chompChar (plusPtr pos 1) end row (col + 1) 0 placeholder of
        Good newPos newCol numChars mostRecent ->
          if numChars /= 1 then
            cerr row col (toError (E.CharNotString (fromIntegral (newCol - col))))
          else
            let
              !newState = P.State src newPos end indent row newCol
              !char = ES.fromChunks [mostRecent]
            in
            cok char newState

        CharEndless newCol ->
          cerr row newCol (toError E.CharEndless)

        CharEscape r c escape ->
          cerr r c (toError (E.CharEscape escape))


data CharResult
  = Good (Ptr Word8) Col Word16 ES.Chunk
  | CharEndless Col
  | CharEscape Row Col E.Escape


chompChar :: Ptr Word8 -> Ptr Word8 -> Row -> Col -> Word16 -> ES.Chunk -> CharResult
chompChar pos end row col numChars mostRecent =
  if pos >= end then
    CharEndless col

  else
    let
      !word = P.unsafeIndex pos
    in
      if word == 0x27 {- ' -} then
        Good (plusPtr pos 1) (col + 1) numChars mostRecent

      else if word == 0x0A {- \n -} then
        CharEndless col

      else if word == 0x22 {- " -} then
        chompChar (plusPtr pos 1) end row (col + 1) (numChars + 1) doubleQuote

      else if word == 0x5C {- \ -} then
        case eatEscape (plusPtr pos 1) end row col of
          EscapeNormal ->
            chompChar (plusPtr pos 2) end row (col + 2) (numChars + 1) (ES.Slice pos 2)

          EscapeUnicode delta code ->
            chompChar (plusPtr pos delta) end row (col + fromIntegral delta) (numChars + 1) (ES.CodePoint code)

          EscapeProblem r c badEscape ->
            CharEscape r c badEscape

          EscapeEndOfFile ->
            CharEndless col

      else
        let
          !width = P.getCharWidth word
          !newPos = plusPtr pos width
        in
        chompChar newPos end row (col + 1) (numChars + 1) (ES.Slice pos width)



-- STRINGS


string :: (Row -> Col -> x) -> (E.String -> Row -> Col -> x) -> Parser x ES.String
string toExpectation toError =
  P.Parser $ \(P.State src pos end indent row col) cok _ cerr eerr ->
    if isDoubleQuote pos end then

      let
        !pos1 = plusPtr pos 1
      in
      case
        if isDoubleQuote pos1 end then
          let !pos2 = plusPtr pos 2 in
          if isDoubleQuote pos2 end then
            let
              !pos3 = plusPtr pos 3
              !col3 = col + 3
            in
            multiString pos3 end row col3 pos3 row col mempty
          else
            Ok pos2 row (col + 2) Utf8.empty
        else
          singleString pos1 end row (col + 1) pos1 mempty
      of
        Ok newPos newRow newCol utf8 ->
          let
            !newState =
              P.State src newPos end indent newRow newCol
          in
          cok utf8 newState

        Err r c x ->
          cerr r c (toError x)

    else
      eerr row col toExpectation


{-# INLINE isDoubleQuote #-}
isDoubleQuote :: Ptr Word8 -> Ptr Word8 -> Bool
isDoubleQuote pos end =
  pos < end && P.unsafeIndex pos == 0x22 {- " -}


data StringResult
  = Ok (Ptr Word8) Row Col !ES.String
  | Err Row Col E.String


finalize :: Ptr Word8 -> Ptr Word8 -> [ES.Chunk] -> ES.String
finalize start end revChunks =
  ES.fromChunks $ reverse $
    if start == end then
      revChunks
    else
      ES.Slice start (minusPtr end start) : revChunks


addEscape :: ES.Chunk -> Ptr Word8 -> Ptr Word8 -> [ES.Chunk] -> [ES.Chunk]
addEscape chunk start end revChunks =
  if start == end then
    chunk : revChunks
  else
    chunk : ES.Slice start (minusPtr end start) : revChunks



-- SINGLE STRINGS


singleString :: Ptr Word8 -> Ptr Word8 -> Row -> Col -> Ptr Word8 -> [ES.Chunk] -> StringResult
singleString pos end row col initialPos revChunks =
  if pos >= end then
    Err row col E.StringEndless_Single

  else
    let
      !word = P.unsafeIndex pos
    in
      if word == 0x22 {- " -} then
        Ok (plusPtr pos 1) row (col + 1) $
          finalize initialPos pos revChunks

      else if word == 0x0A {- \n -} then
        Err row col E.StringEndless_Single

      else if word == 0x27 {- ' -} then
        let !newPos = plusPtr pos 1 in
        singleString newPos end row (col + 1) newPos $
          addEscape singleQuote initialPos pos revChunks

      else if word == 0x5C {- \ -} then
        case eatEscape (plusPtr pos 1) end row col of
          EscapeNormal ->
            singleString (plusPtr pos 2) end row (col + 2) initialPos revChunks

          EscapeUnicode delta code ->
            let !newPos = plusPtr pos delta in
            singleString newPos end row (col + fromIntegral delta) newPos $
              addEscape (ES.CodePoint code) initialPos pos revChunks

          EscapeProblem r c x ->
            Err r c (E.StringEscape x)

          EscapeEndOfFile ->
            Err row (col + 1) E.StringEndless_Single

      else
        let !newPos = plusPtr pos (P.getCharWidth word) in
        singleString newPos end row (col + 1) initialPos revChunks



-- MULTI STRINGS


multiString :: Ptr Word8 -> Ptr Word8 -> Row -> Col -> Ptr Word8 -> Row -> Col -> [ES.Chunk] -> StringResult
multiString pos end row col initialPos sr sc revChunks =
  if pos >= end then
    Err sr sc E.StringEndless_Multi

  else
    let !word = P.unsafeIndex pos in
    if word == 0x22 {- " -} && isDoubleQuote (plusPtr pos 1) end && isDoubleQuote (plusPtr pos 2) end then
      Ok (plusPtr pos 3) row (col + 3) $
        finalize initialPos pos revChunks

    else if word == 0x27 {- ' -} then
      let !pos1 = plusPtr pos 1 in
      multiString pos1 end row (col + 1) pos1 sr sc $
        addEscape singleQuote initialPos pos revChunks

    else if word == 0x0A {- \n -} then
      let !pos1 = plusPtr pos 1 in
      multiString pos1 end (row + 1) 1 pos1 sr sc $
        addEscape newline initialPos pos revChunks

    else if word == 0x0D {- \r -} then
      let !pos1 = plusPtr pos 1 in
      multiString pos1 end row col pos1 sr sc $
        addEscape carriageReturn initialPos pos revChunks

    else if word == 0x5C {- \ -} then
      case eatEscape (plusPtr pos 1) end row col of
        EscapeNormal ->
          multiString (plusPtr pos 2) end row (col + 2) initialPos sr sc revChunks

        EscapeUnicode delta code ->
          let !newPos = plusPtr pos delta in
          multiString newPos end row (col + fromIntegral delta) newPos sr sc $
            addEscape (ES.CodePoint code) initialPos pos revChunks

        EscapeProblem r c x ->
          Err r c (E.StringEscape x)

        EscapeEndOfFile ->
          Err sr sc E.StringEndless_Multi

    else
      let !newPos = plusPtr pos (P.getCharWidth word) in
      multiString newPos end row (col + 1) initialPos sr sc revChunks



-- ESCAPE CHARACTERS


data Escape
  = EscapeNormal
  | EscapeUnicode !Int !Int
  | EscapeEndOfFile
  | EscapeProblem Row Col E.Escape


eatEscape :: Ptr Word8 -> Ptr Word8 -> Row -> Col -> Escape
eatEscape pos end row col =
  if pos >= end then
    EscapeEndOfFile

  else
    case P.unsafeIndex pos of
      0x6E {- n -} -> EscapeNormal
      0x72 {- r -} -> EscapeNormal
      0x74 {- t -} -> EscapeNormal
      0x22 {- " -} -> EscapeNormal
      0x27 {- ' -} -> EscapeNormal
      0x5C {- \ -} -> EscapeNormal
      0x75 {- u -} -> eatUnicode (plusPtr pos 1) end row col
      _            -> EscapeProblem row col E.EscapeUnknown


eatUnicode :: Ptr Word8 -> Ptr Word8 -> Row -> Col -> Escape
eatUnicode pos end row col =
  if pos >= end || P.unsafeIndex pos /= 0x7B {- { -} then
    EscapeProblem row col (E.BadUnicodeFormat 2)
  else
    let
      !digitPos = plusPtr pos 1
      (# newPos, code #) = Number.chompHex digitPos end
      !numDigits = minusPtr newPos digitPos
    in
    if newPos >= end || P.unsafeIndex newPos /= 0x7D {- } -} then
      EscapeProblem row col $ E.BadUnicodeFormat (2 + fromIntegral (minusPtr newPos pos))

    else if code < 0 || 0x10FFFF < code then
      EscapeProblem row col $ E.BadUnicodeCode (3 + fromIntegral (minusPtr newPos pos))

    else if numDigits < 4 || 6 < numDigits then
      EscapeProblem row col $
        E.BadUnicodeLength
          (3 + fromIntegral (minusPtr newPos pos))
          numDigits
          code

    else
      EscapeUnicode (numDigits + 4) code


{-# NOINLINE singleQuote #-}
singleQuote :: ES.Chunk
singleQuote =
  ES.Escape 0x27 {-'-}


{-# NOINLINE doubleQuote #-}
doubleQuote :: ES.Chunk
doubleQuote =
  ES.Escape 0x22 {-"-}


{-# NOINLINE newline #-}
newline :: ES.Chunk
newline =
  ES.Escape 0x6E {-n-}


{-# NOINLINE carriageReturn #-}
carriageReturn :: ES.Chunk
carriageReturn =
  ES.Escape 0x72 {-r-}


{-# NOINLINE placeholder #-}
placeholder :: ES.Chunk
placeholder =
  ES.CodePoint 0xFFFD {-replacement character-}
