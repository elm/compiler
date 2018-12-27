{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE BangPatterns, MagicHash, OverloadedStrings, UnboxedTuples #-}
module Parse.Utf8
  ( string
  , character
  )
  where


import qualified Data.Utf8 as Utf8
import Data.Word (Word8, Word16)
import Foreign.Ptr (Ptr, plusPtr, minusPtr)

import Parse.Utils (Parser)
import qualified Parse.Number as Number
import qualified Parse.Primitives as P
import qualified Reporting.Error.Syntax as E



-- CHARACTER


character :: Parser Utf8.String
character =
  P.Parser $ \(P.State pos end indent row col ctx) cok _ cerr eerr ->
    if pos >= end || P.unsafeIndex pos /= 0x27 {- ' -} then
      eerr row col ctx E.CharStart

    else
      case chompChar (plusPtr pos 1) end (col + 1) 0 placeholder of
        Good newPos newCol numChars mostRecent ->
          if numChars /= 1 then
            cerr row col ctx (E.CharNotString col newCol)
          else
            let
              !newState = P.State newPos end indent row newCol ctx
              !char = Utf8.fromChunks [mostRecent]
            in
            cok char newState

        Bad newCol x ->
          cerr row newCol ctx x


data CharResult
  = Good (Ptr Word8) Word16 Int Utf8.Chunk
  | Bad Word16 E.Expectation


chompChar :: Ptr Word8 -> Ptr Word8 -> Word16 -> Int -> Utf8.Chunk -> CharResult
chompChar pos end col numChars mostRecent =
  if pos >= end then
    Bad col E.CharEnd

  else
    let
      !word = P.unsafeIndex pos
    in
      if word == 0x27 {- ' -} then
        Good (plusPtr pos 1) (col + 1) numChars mostRecent

      else if word == 0x0A {- \n -} then
        Bad col E.CharEnd

      else if word == 0x22 {- " -} then
        chompChar (plusPtr pos 1) end (col + 1) (numChars + 1) doubleQuote

      else if word == 0x5C {- \ -} then
        case eatEscape (plusPtr pos 1) end col of
          EscapeNormal ->
            chompChar (plusPtr pos 2) end (col + 2) (numChars + 1) (Utf8.Slice pos 2)

          EscapeUnicode delta code ->
            chompChar (plusPtr pos delta) end (col + fromIntegral delta) (numChars + 1) (Utf8.CodePoint code)

          EscapeProblem x ->
            Bad col x

          EscapeEndOfFile ->
            Bad (col + 1) E.CharEnd

      else
        let
          !width = P.getCharWidth pos end word
          !newPos = plusPtr pos width
        in
        chompChar newPos end (col + 1) (numChars + 1) (Utf8.Slice pos width)



-- STRINGS


string :: Parser Utf8.String
string =
  P.Parser $ \(P.State pos end indent row col ctx) cok _ cerr eerr ->
    if isDoubleQuote pos end then

      let
        !pos1 = plusPtr pos 1
        result =
          if isDoubleQuote pos1 end && isDoubleQuote (plusPtr pos 2) end then
            multiString (plusPtr pos 3) end row (col + 3) (plusPtr pos 3) mempty
          else
            singleString pos1 end row (col + 1) pos1 mempty
      in
        case result of
          Ok newPos newRow newCol utf8 ->
            let
              !newState =
                P.State newPos end indent newRow newCol ctx
            in
            cok utf8 newState

          Err r c x ->
            cerr r c ctx x

    else
      eerr row col ctx E.StringStart


{-# INLINE isDoubleQuote #-}
isDoubleQuote :: Ptr Word8 -> Ptr Word8 -> Bool
isDoubleQuote pos end =
  pos < end && P.unsafeIndex pos == 0x22 {- " -}


data StringResult
  = Ok (Ptr Word8) Word16 Word16 Utf8.String
  | Err Word16 Word16 E.Expectation


finalize :: Ptr Word8 -> Ptr Word8 -> [Utf8.Chunk] -> Utf8.String
finalize start end revChunks =
  Utf8.fromChunks $ reverse $
    if start == end then
      revChunks
    else
      Utf8.Slice start (minusPtr end start) : revChunks


addEscape :: Utf8.Chunk -> Ptr Word8 -> Ptr Word8 -> [Utf8.Chunk] -> [Utf8.Chunk]
addEscape chunk start end revChunks =
  if start == end then
    chunk : revChunks
  else
    chunk : Utf8.Slice start (minusPtr end start) : revChunks



-- SINGLE STRINGS


singleString :: Ptr Word8 -> Ptr Word8 -> Word16 -> Word16 -> Ptr Word8 -> [Utf8.Chunk] -> StringResult
singleString pos end row col initialPos revChunks =
  if pos >= end then
    Err row col E.StringEnd_Single

  else
    let
      !word = P.unsafeIndex pos
    in
      if word == 0x22 {- " -} then
        Ok (plusPtr pos 1) row (col + 1) $
          finalize initialPos pos revChunks

      else if word == 0x0A {- \n -} then
        Err row col E.StringEnd_Single

      else if word == 0x27 {- ' -} then
        let !newPos = plusPtr pos 1 in
        singleString newPos end row (col + 1) newPos $
          addEscape singleQuote initialPos pos revChunks

      else if word == 0x5C {- \ -} then
        case eatEscape (plusPtr pos 1) end col of
          EscapeNormal ->
            singleString (plusPtr pos 2) end row (col + 2) initialPos revChunks

          EscapeUnicode delta code ->
            let !newPos = plusPtr pos delta in
            singleString newPos end row (col + fromIntegral delta) newPos $
              addEscape (Utf8.CodePoint code) initialPos pos revChunks

          EscapeProblem x ->
            Err row col x

          EscapeEndOfFile ->
            Err row (col + 1) E.StringEnd_Single

      else
        let !newPos = plusPtr pos (P.getCharWidth pos end word) in
        singleString newPos end row (col + 1) initialPos revChunks



-- MULTI STRINGS


multiString :: Ptr Word8 -> Ptr Word8 -> Word16 -> Word16 -> Ptr Word8 -> [Utf8.Chunk] -> StringResult
multiString pos end row col initialPos revChunks =
  if pos >= end then
    Err row col E.StringEnd_Multi

  else
    let !word = P.unsafeIndex pos in
    if word == 0x22 {- " -} && isDoubleQuote (plusPtr pos 1) end && isDoubleQuote (plusPtr pos 2) end then
      Ok (plusPtr pos 3) row (col + 3) $
        finalize initialPos pos revChunks

    else if word == 0x27 {- ' -} then
      let !pos1 = plusPtr pos 1 in
      multiString pos1 end row (col + 1) pos1 $
        addEscape singleQuote initialPos pos revChunks

    else if word == 0x0A {- \n -} then
      let !pos1 = plusPtr pos 1 in
      multiString pos1 end (row + 1) 1 pos1 $
        addEscape newline initialPos pos revChunks

    else if word == 0x0D {- \r -} then
      let !pos1 = plusPtr pos 1 in
      multiString pos1 end row col pos1 $
        addEscape carriageReturn initialPos pos revChunks

    else if word == 0x5C {- \ -} then
      case eatEscape (plusPtr pos 1) end col of
        EscapeNormal ->
          multiString (plusPtr pos 2) end row (col + 2) initialPos revChunks

        EscapeUnicode delta code ->
          let !newPos = plusPtr pos delta in
          multiString newPos end row (col + fromIntegral delta) newPos $
            addEscape (Utf8.CodePoint code) initialPos pos revChunks

        EscapeProblem x ->
          Err row col x

        EscapeEndOfFile ->
          Err row (col + 1) E.StringEnd_Multi

    else
      let !newPos = plusPtr pos (P.getCharWidth pos end word) in
      multiString newPos end row (col + 1) initialPos revChunks



-- ESCAPE CHARACTERS


data Escape
  = EscapeNormal
  | EscapeUnicode !Int !Int
  | EscapeEndOfFile
  | EscapeProblem E.Expectation


eatEscape :: Ptr Word8 -> Ptr Word8 -> Word16 -> Escape
eatEscape pos end startCol =
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
      0x75 {- u -} -> eatUnicode (plusPtr pos 1) end startCol
      _            -> EscapeProblem (E.EscapeUnknown startCol (startCol + 1))


eatUnicode :: Ptr Word8 -> Ptr Word8 -> Word16 -> Escape
eatUnicode pos end startCol =
  if pos >= end || P.unsafeIndex pos /= 0x7B {- { -} then
    EscapeProblem $ E.BadUnicodeStart startCol (startCol + 1)
  else
    let
      !digitPos = plusPtr pos 1
      (# newPos, code #) = Number.chompHex digitPos end
      !numDigits = minusPtr newPos digitPos
    in
    if newPos >= end || P.unsafeIndex newPos /= 0x7D {- } -} then
      EscapeProblem $ E.BadUnicodeEnd startCol $
        startCol + fromIntegral (minusPtr newPos pos)

    else if code < 0 || 0x10FFFF < code then
      EscapeProblem $ E.BadUnicodeRange startCol $
        startCol + fromIntegral (minusPtr newPos pos) + 1

    else if numDigits < 4 || 6 < numDigits then
      EscapeProblem $
        E.BadUnicodeFormat
          startCol
          (startCol + fromIntegral (minusPtr newPos pos) + 1)
          numDigits
          (Utf8.fromPtr digitPos newPos)

    else
      EscapeUnicode (numDigits + 4) code


{-# NOINLINE singleQuote #-}
singleQuote :: Utf8.Chunk
singleQuote =
  Utf8.Escape 0x27 {- ' -}


{-# NOINLINE doubleQuote #-}
doubleQuote :: Utf8.Chunk
doubleQuote =
  Utf8.Escape 0x22 {- " -}


{-# NOINLINE newline #-}
newline :: Utf8.Chunk
newline =
  Utf8.Escape 0x6E {- n -}


{-# NOINLINE carriageReturn #-}
carriageReturn :: Utf8.Chunk
carriageReturn =
  Utf8.Escape 0x72 {- \r -}


{-# NOINLINE placeholder #-}
placeholder :: Utf8.Chunk
placeholder =
  Utf8.CodePoint 0xFFFD {- replacement character -}
