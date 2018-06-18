{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE BangPatterns, MagicHash, OverloadedStrings, UnboxedTuples #-}
module Parse.Primitives.Utf8
  ( string
  , character
  )
  where


import Prelude hiding (length)
import Data.Bits
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.UTF8 as Utf8
import qualified Data.Char as Char
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as Text
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Word (Word8(..))

import Parse.Primitives.Internals (Parser(..), State(..), noError)
import qualified Parse.Primitives.Internals as I
import qualified Parse.Primitives.Number as Number
import qualified Reporting.Error.Syntax as E



-- CHARACTER


character :: Parser Text.Text
character =
  Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ eerr ->
    if offset >= terminal || I.unsafeIndex fp offset /= 0x27 {- ' -} then
      eerr noError

    else
      case chompChar fp (offset + 1) terminal (col + 1) 0 "" of
        Bad newCol problem ->
          cerr (E.ParseError row newCol problem)

        Good newOffset newCol numChars mostRecent ->
          if numChars /= 1 then
            cerr (E.ParseError row col (E.BadChar newCol))
          else
            let !newState = State fp newOffset terminal indent row newCol ctx in
            cok mostRecent newState noError


data CharResult
  = Bad Int E.Problem
  | Good Int Int Int Text.Text


chompChar :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Text.Text -> CharResult
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
        chompChar fp (offset + 1) terminal (col + 1) (numChars + 1) "\\\""

      else if word == 0x5C {- \ -} then
        case eatEscape fp (offset + 1) terminal of
          EscapeNormal ->
            chompChar fp (offset + 2) terminal (col + 2) (numChars + 1) (toText fp offset 2)

          EscapeUnicode delta bits ->
            chompChar fp (offset + delta) terminal (col + delta) (numChars + 1) (Text.pack bits)

          EscapeProblem newOffset problem ->
            Bad col (E.BadEscape (newOffset - offset) problem)

          EscapeEndOfFile ->
            Bad col E.EndOfFile_Char

      else
        let !width = I.getCharWidth fp offset terminal word in
        chompChar fp (offset + width) terminal (col + 1) (numChars + 1) (toText fp offset width)


toText :: ForeignPtr Word8 -> Int -> Int -> Text.Text
toText fp offset length =
  Text.decodeUtf8 (B.PS fp offset length)



-- STRINGS


string :: Parser Text.Text
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

          Ok newOffset newRow newCol builder ->
            let
              !newState = State fp newOffset terminal indent newRow newCol ctx
              !content = LText.toStrict (Text.toLazyText builder)
            in
              cok content newState noError

    else
      eerr noError


{-# INLINE isDoubleQuote #-}
isDoubleQuote :: ForeignPtr Word8 -> Int -> Int -> Bool
isDoubleQuote fp offset terminal =
  offset < terminal && I.unsafeIndex fp offset == 0x22 {- " -}


data StringResult
  = Err E.ParseError
  | Ok !Int !Int !Int Text.Builder


finalize :: ForeignPtr Word8 -> Int -> Int -> Text.Builder -> Text.Builder
finalize fp start end builder =
  if start == end then
    builder
  else
    builder <> Text.fromText (Text.decodeUtf8 (B.PS fp start (end - start)))


addBits :: Text.Builder -> ForeignPtr Word8 -> Int -> Int -> Text.Builder -> Text.Builder
addBits bits fp start end builder =
  if start == end then
    builder <> bits
  else
    builder <> Text.fromText (Text.decodeUtf8 (B.PS fp start (end - start))) <> bits



-- SINGLE STRINGS


singleString :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Int -> Text.Builder -> StringResult
singleString fp offset terminal row col initialOffset builder =
  if offset >= terminal then
    Err (E.ParseError row col E.EndOfFile_String)

  else
    let
      !word = I.unsafeIndex fp offset
    in
      if word == 0x22 {- " -} then
        Ok (offset + 1) row (col + 1) $
          finalize fp initialOffset offset builder

      else if word == 0x0A {- \n -} then
        Err (E.ParseError row col E.NewLineInString)

      else if word == 0x27 {- ' -} then
        let !newOffset = offset + 1 in
        singleString fp newOffset terminal row (col + 1) newOffset $
          addBits singleQuoteBits fp initialOffset offset builder

      else if word == 0x5C {- \ -} then
        case eatEscape fp (offset + 1) terminal of
          EscapeNormal ->
            singleString fp (offset + 2) terminal row (col + 2) initialOffset builder

          EscapeUnicode delta bits ->
            let !newOffset = offset + delta in
            singleString fp newOffset terminal row (col + delta) newOffset $
              addBits (Text.fromText (Text.pack bits)) fp initialOffset offset builder

          EscapeProblem newOffset problem ->
            Err (E.ParseError row col (E.BadEscape (newOffset - offset) problem))

          EscapeEndOfFile ->
            Err (E.ParseError row (col + 1) E.EndOfFile_String)

      else
        let !newOffset = offset + I.getCharWidth fp offset terminal word in
        singleString fp newOffset terminal row (col + 1) initialOffset builder



-- MULTI STRINGS


multiString :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Int -> Text.Builder -> StringResult
multiString fp offset terminal row col initialOffset builder =
  if offset >= terminal then
    Err (E.ParseError row col E.EndOfFile_MultiString)

  else
    let !word = I.unsafeIndex fp offset in
    if word == 0x22 {- " -} && isDoubleQuote fp (offset + 1) terminal && isDoubleQuote fp (offset + 2) terminal then
      Ok (offset + 3) row (col + 3) $
        finalize fp initialOffset offset builder

    else if word == 0x27 {- ' -} then
      let !offset1 = offset + 1 in
      multiString fp offset1 terminal row (col + 1) offset1 $
        addBits singleQuoteBits fp initialOffset offset builder

    else if word == 0x0A {- \n -} then
      let !offset1 = offset + 1 in
      multiString fp offset1 terminal (row + 1) 1 offset1 $
        addBits newlineBits fp initialOffset offset builder

    else if word == 0x0D {- \r -} then
      let !offset1 = offset + 1 in
      multiString fp offset1 terminal (row + 1) 1 offset1 $
        addBits carriageReturnBits fp initialOffset offset builder

    else if word == 0x5C {- \ -} then
      case eatEscape fp (offset + 1) terminal of
        EscapeNormal ->
          multiString fp (offset + 2) terminal row (col + 2) initialOffset builder

        EscapeUnicode delta bits ->
          let !newOffset = offset + delta in
          multiString fp newOffset terminal row (col + delta) newOffset $
            addBits (Text.fromText (Text.pack bits)) fp initialOffset offset builder

        EscapeProblem newOffset problem ->
          Err (E.ParseError row col (E.BadEscape (newOffset - offset) problem))

        EscapeEndOfFile ->
          Err (E.ParseError row (col + 1) E.EndOfFile_MultiString)

    else
      let !newOffset = offset + I.getCharWidth fp offset terminal word in
      multiString fp newOffset terminal row (col + 1) initialOffset builder



-- ESCAPE CHARACTERS


data Escape
  = EscapeNormal
  | EscapeUnicode !Int [Char]
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
        E.UnicodeLength numDigits (Utf8.toString (B.PS fp digitOffset numDigits))

    else
      EscapeUnicode (numDigits + 4) (codePointToBits code)



-- CODE POINT TO BITS


codePointToBits :: Int -> [Char]
codePointToBits code =
  if code < 0xFFFF then
    wordToBits code

  else
    let
      (hi,lo) = divMod (code - 0x10000) 0x400
    in
    wordToBits (hi + 0xD800) ++ wordToBits (lo + 0xDC00)


wordToBits :: Int -> [Char]
wordToBits code =
  [ '\\' -- 0x5C -- \
  , 'u'  -- 0x75 -- u
  , toBits code 12
  , toBits code 8
  , toBits code 4
  , toBits code 0
  ]


toBits :: Int -> Int -> Char
toBits code offset =
  let !n = fromIntegral (shiftR code offset .&. 0x000F) in
  Char.chr $ if n < 10 then 0x30 + n else 0x61 + (n - 10)


{-# NOINLINE singleQuoteBits #-}
singleQuoteBits :: Text.Builder
singleQuoteBits =
  "\\\'"


{-# NOINLINE newlineBits #-}
newlineBits :: Text.Builder
newlineBits =
  "\\n"


{-# NOINLINE carriageReturnBits #-}
carriageReturnBits :: Text.Builder
carriageReturnBits =
  "\\r"



---- CHUNKS


--data Chunk
--  = Copy { _offset :: Int, _length :: Int }
--  | Bits [Word8]


--finalize :: Int -> Int -> [Chunk] -> [Chunk]
--finalize start end chunks =
--  reverse $
--    if start == end then
--      chunks
--    else
--      Copy start (end - start) : chunks


--addBits :: [Word8] -> Int -> Int -> [Chunk] -> [Chunk]
--addBits bits start end chunks =
--  if start == end then
--    Bits bits : chunks
--  else
--    Bits bits : Copy start (end - start) : chunks


--chunkLength :: Chunk -> Int
--chunkLength chunk =
--  case chunk of
--    Copy _ len ->
--      len

--    Bits bits ->
--      length bits



---- CHUNK TO SHORT


--chunkToShort :: ForeignPtr Word8 -> Chunk -> S.ShortByteString
--chunkToShort fp chunk =
--  unsafeDupablePerformIO $ stToIO $
--    do  MBA mba <- newMutableByteArray (chunkLength chunk)
--        case chunk of
--          Copy offset len ->
--            do  copyToByteArray (unsafeForeignPtrToPtr fp `plusPtr` offset) mba 0 len
--                freeze mba

--          Bits bits ->
--            do  _ <- writeBits mba 0 bits
--                freeze mba



---- CHUNKS TO SHORT


--chunksToShort :: ForeignPtr Word8 -> [Chunk] -> S.ShortByteString
--chunksToShort fp chunks =
--  unsafeDupablePerformIO $ stToIO $
--    do  MBA mba <- newMutableByteArray (sum (map chunkLength chunks))
--        chunksToShortHelp fp mba 0 chunks


--chunksToShortHelp :: ForeignPtr Word8 -> Prim.MutableByteArray# s -> Int -> [Chunk] -> ST s S.ShortByteString
--chunksToShortHelp fp mba index chunks =
--  case chunks of
--    [] ->
--      freeze mba

--    chunk : others ->
--      case chunk of
--        Copy offset len ->
--          do  copyToByteArray (unsafeForeignPtrToPtr fp `plusPtr` offset) mba index len
--              chunksToShortHelp fp mba (index + len) others

--        Bits bits ->
--          do  newIndex <- writeBits mba index bits
--              chunksToShortHelp fp mba newIndex others



---- CHUNKS HELPERS


--data MBA s = MBA (Prim.MutableByteArray# s)


--newMutableByteArray :: Int -> ST s (MBA s)
--newMutableByteArray (I# len) =
--  ST $ \s1 ->
--    case Prim.newByteArray# len s1 of
--      (# s2, mba #) -> (# s2, MBA mba #)


--freeze :: Prim.MutableByteArray# s -> ST s S.ShortByteString
--freeze mba =
--  ST $ \s1 ->
--    case Prim.unsafeFreezeByteArray# mba s1 of
--      (# s2, ba #) -> (# s2, S.SBS ba #)


--copyToByteArray :: Ptr Word8 -> Prim.MutableByteArray# s -> Int -> Int -> ST s ()
--copyToByteArray (Ptr src) dst (I# dstOffset) (I# len) =
--    ST $ \s1 ->
--      case Prim.copyAddrToByteArray# src dst dstOffset len s1 of
--        s2 -> (# s2, () #)


--writeBits :: Prim.MutableByteArray# s -> Int -> [Word8] -> ST s Int
--writeBits mba index bits =
--  case bits of
--    [] ->
--      return index

--    word : others ->
--      do  writeWord8Array mba index word
--          writeBits mba (index + 1) others


--writeWord8Array :: Prim.MutableByteArray# s -> Int -> Word8 -> ST s ()
--writeWord8Array mba (I# offset) (W8# word) =
--  ST $ \s1 ->
--    case Prim.writeWord8Array# mba offset word s1 of
--      s2 -> (# s2, () #)
