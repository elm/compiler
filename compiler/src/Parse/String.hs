{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, QuasiQuotes, UnboxedTuples,
UnliftedDatatypes
#-}
module Parse.String
  ( string
  , character
  )
  where


import qualified Data.List as List
import qualified Data.Utf8 as Utf8
import GHC.Base (Char(C#), Word(W#), UnliftedType)
import GHC.Exts (isTrue#)
import GHC.Int (Int(..))
import GHC.Prim

import qualified Literals

import qualified Elm.String as ES
import Parse.Primitives (Parser, Cursor, slide, newline, ltAddr, leAddr, notLtAddr, eqIndex, neIndex)
import qualified Parse.Number as Number
import qualified Parse.Primitives as P
import qualified Reporting.Error.Syntax as E



-- CHARACTER


character :: (Cursor -> x) -> (E.Char -> Cursor -> x) -> Parser x Char
character toExpectation toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ cerr eerr ->
    if notLtAddr pos end || neIndex pos 0# 0x27#Word8 {-'-} then
      eerr cur toExpectation

    else
      case chompChar (plusAddr# pos 1#) end (slide cur 1#Word64) of
        Good newPos newCur numChars latestChar ->
          case numChars of
            1# ->
              do  let !newState = P.State newPos end indent newCur
                  cok latestChar newState

            _ ->
              cerr cur (toError (E.CharNotString (subWord64# newCur cur)))

        CharEscape  c e -> cerr c (toError (E.CharEscape e))
        CharEndless c   -> cerr c (toError E.CharEndless)
        CharNotUtf8 c   -> cerr c (toError E.CharNotUtf8)



-- CHOMP CHAR


type CharResult :: UnliftedType
data CharResult
  = Good Addr# Cursor Int# Char
  | CharEscape  Cursor E.Escape
  | CharEndless Cursor
  | CharNotUtf8 Cursor


chompChar :: Addr# -> Addr# -> Cursor -> CharResult
chompChar start end cursor0 =
    loop start cursor0 0# placeholder
  where
    loop pos cur numChars latestChar =
      if ltAddr pos end
      then
        case indexWord8OffAddr# pos 0# of
          w
            | isTrue# (ltWord8# w 0x80#Word8) -> loop1 w
            | isTrue# (ltWord8# w 0xC0#Word8) -> CharNotUtf8 cur
            | isTrue# (ltWord8# w 0xE0#Word8) -> loopN (char2 pos end w)
            | isTrue# (ltWord8# w 0xF0#Word8) -> loopN (char3 pos end w)
            | isTrue# (ltWord8# w 0xF8#Word8) -> loopN (char4 pos end w)
            | otherwise                       -> CharNotUtf8 cur
      else
        CharEndless cur
      where
        loop1 w =
          case w of
            0x27#Word8 {-'-}  -> Good (plusAddr# pos 1#) (slide cur 1#Word64) numChars latestChar
            0x0A#Word8 {-\n-} -> CharEndless cur
            0x5C#Word8 {-\-}  ->
              case eatEscape (plusAddr# pos 1#) end cur of
                EscapeUnicode delta code ->
                  loop (plusAddr# pos delta) (slide cur (wordToWord64# (int2Word# delta))) (numChars +# 1#) (C# (chr# (word2Int# code)))

                EscapeProblem c badEscape ->
                  CharEscape c badEscape

                EscapeEndOfFile ->
                  CharEndless cur

            _ ->
              loop (plusAddr# pos 1#) (slide cur 1#Word64) (numChars +# 1#) (C# (chr# (word2Int# (word8ToWord# w))))

        loopN (# newPos, char #) =
          if ltAddr pos newPos
          then loop newPos (slide cur 1#Word64) (numChars +# 1#) char
          else CharNotUtf8 cur


placeholder :: Char
placeholder =
  C# (chr# 0xFFFD#) {- replacement character -}



-- STRINGS


string :: (Cursor -> x) -> (E.String -> Cursor -> x) -> Parser x ES.String
string toExpectation toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ cerr eerr ->
    if isDoubleQuote pos end then
      let
        !pos1 = plusAddr# pos 1#
      in
      case
        if isDoubleQuote pos1 end then
          let !pos2 = plusAddr# pos 2# in
          if isDoubleQuote pos2 end
          then multiString pos end cur
          else Ok pos2 (slide cur 2#Word64) Utf8.empty
        else
          singleString pos1 end (slide cur 1#Word64)
      of
        Ok newPos newCur utf8 ->
          let
            !newState =
              P.State newPos end indent newCur
          in
          cok utf8 newState

        Err c x ->
          cerr c (toError x)

    else
      eerr cur toExpectation


{-# INLINE isDoubleQuote #-}
isDoubleQuote :: Addr# -> Addr# -> Bool
isDoubleQuote pos end =
  ltAddr pos end && eqIndex pos 0# 0x22#Word8 {-"-}



-- STRING HELPERS


type StringResult :: UnliftedType
data StringResult
  = Ok Addr# Cursor !ES.String
  | Err Cursor E.String


finalize :: Addr# -> Addr# -> [ES.Chunk] -> ES.String
finalize start end revChunks =
  ES.fromChunks $ List.reverse $
    if isTrue# (eqAddr# start end)
    then revChunks
    else ES.Slice start (minusAddr# end start) : revChunks


addEscape :: ES.Chunk -> Addr# -> Addr# -> [ES.Chunk] -> [ES.Chunk]
addEscape chunk start end revChunks =
  if isTrue# (eqAddr# start end)
  then chunk : revChunks
  else chunk : ES.Slice start (minusAddr# end start) : revChunks



-- SINGLE STRINGS


singleString :: Addr# -> Addr# -> Cursor -> StringResult
singleString start end cursor0 =
    loop start cursor0 0#Word8 start []
  where
    !(Literals.FSM table) = string_fsm

    loop pos cur state chunk0 revChunks =
      if ltAddr pos end
      then
        let
          !w = indexWord8OffAddr# pos 0#
        in
        case step table (word8ToWord# w) state of
          0#Word8 ->
            loop (plusAddr# pos 1#) (slide cur 1#Word64) 0#Word8 chunk0 revChunks

          117#Word8 ->
            case w of
              0x22#Word8 {-"-} ->
                Ok (plusAddr# pos 1#) (slide cur 1#Word64) $
                  finalize chunk0 pos revChunks

              0x27#Word8 {-'-} ->
                let !newPos = plusAddr# pos 1# in
                loop newPos (slide cur 1#Word64) 0#Word8 newPos $
                  addEscape (ES.CodePoint 0x27## {-'-}) chunk0 pos revChunks

              0x5C#Word8 {-\-} ->
                case eatEscape (plusAddr# pos 1#) end cur of
                  EscapeUnicode delta code ->
                    let !newPos = plusAddr# pos delta in
                    loop newPos (slide cur (wordToWord64# (int2Word# delta))) 0#Word8 newPos $
                      addEscape (ES.CodePoint code) chunk0 pos revChunks

                  EscapeProblem c x ->
                    Err c (E.StringEscape x)

                  EscapeEndOfFile ->
                    Err (slide cur 1#Word64) E.StringEndless_Single

              0x0A#Word8 {-\n-} ->
                Err cur E.StringEndless_Single

              _ ->
                loop (plusAddr# pos 1#) (slide cur 1#Word64) 0#Word8 chunk0 revChunks

          13#Word8 ->
            Err cur E.StringNotUtf8

          s# ->
            loop (plusAddr# pos 1#) cur s# chunk0 revChunks
      else
        Err cur E.StringEndless_Single



-- MULTI STRINGS


multiString :: Addr# -> Addr# -> Cursor -> StringResult
multiString start end cursor0 =
    loop start3# (slide cursor0 3#Word64) 0#Word8 start3# []
  where
    !start3# = plusAddr# start 3#
    !(Literals.FSM table) = string_fsm

    loop pos cur state chunk0 revChunks =
      if ltAddr pos end
      then
        let
          !w = indexWord8OffAddr# pos 0#
        in
        case step table (word8ToWord# w) state of
          0#Word8 ->
            loop (plusAddr# pos 1#) (slide cur 1#Word64) 0#Word8 chunk0 revChunks

          117#Word8 ->
            case w of
              0x22#Word8 {-"-} ->
                let !pos2 = plusAddr# pos 2# in
                if ltAddr pos2 end && isDoubleQuote (plusAddr# pos 1#) end && isDoubleQuote pos2 end
                then
                  Ok (plusAddr# pos 3#) (slide cur 3#Word64) $
                    finalize chunk0 pos revChunks
                else
                  let !pos1 = plusAddr# pos 1# in
                  loop pos1 (slide cur 1#Word64) 0#Word8 pos1 $
                    addEscape (ES.CodePoint 0x22## {-"-}) chunk0 pos revChunks

              0x27#Word8 {-'-} ->
                let !pos1 = plusAddr# pos 1# in
                loop pos1 (slide cur 1#Word64) 0#Word8 pos1 $
                  addEscape (ES.CodePoint 0x27## {-'-}) chunk0 pos revChunks

              0x0A#Word8 {-\n-} ->
                let !pos1 = plusAddr# pos 1# in
                loop pos1 (newline cur) 0#Word8 pos1 $
                  addEscape (ES.CodePoint 0x0A## {-\n-}) chunk0 pos revChunks

              0x0D#Word8 {-\r-} ->
                let !pos1 = plusAddr# pos 1# in
                loop pos1 cur 0#Word8 pos1 $
                  addEscape (ES.CodePoint 0x0D## {-\r-}) chunk0 pos revChunks

              0x5C#Word8 {-\-} ->
                case eatEscape (plusAddr# pos 1#) end cur of
                  EscapeUnicode delta code ->
                    let !newPos = plusAddr# pos delta in
                    loop newPos (slide cur (wordToWord64# (int2Word# delta))) 0#Word8 newPos $
                      addEscape (ES.CodePoint code) chunk0 pos revChunks

                  EscapeProblem c x ->
                    Err c (E.StringEscape x)

                  EscapeEndOfFile ->
                    Err cursor0 E.StringEndless_Multi

              _ ->
                loop (plusAddr# pos 1#) (slide cur 1#Word64) 0#Word8 chunk0 revChunks

          13#Word8 ->
            Err cur E.StringNotUtf8

          s# ->
            loop (plusAddr# pos 1#) cur s# chunk0 revChunks
      else
        Err cursor0 E.StringEndless_Multi



-- ESCAPE CHARACTERS


type Escape :: UnliftedType
data Escape
  = EscapeUnicode Int# Word#
  | EscapeEndOfFile
  | EscapeProblem Cursor E.Escape


eatEscape :: Addr# -> Addr# -> Cursor -> Escape
eatEscape pos end cur =
  if notLtAddr pos end
  then EscapeEndOfFile
  else
    case indexWord8OffAddr# pos 0# of
      0x6E#Word8 {-n-} -> EscapeUnicode 2# 0x0A## {-n-}
      0x72#Word8 {-r-} -> EscapeUnicode 2# 0x0D## {-r-}
      0x74#Word8 {-t-} -> EscapeUnicode 2# 0x09## {-t-}
      0x22#Word8 {-"-} -> EscapeUnicode 2# 0x22## {-"-}
      0x27#Word8 {-'-} -> EscapeUnicode 2# 0x27## {-'-}
      0x5C#Word8 {-\-} -> EscapeUnicode 2# 0x5C## {-\-}
      0x75#Word8 {-u-} -> eatUnicode (plusAddr# pos 1#) end cur
      _                -> EscapeProblem cur E.EscapeUnknown


eatUnicode :: Addr# -> Addr# -> Cursor -> Escape
eatUnicode pos end cur =
  if notLtAddr pos end || neIndex pos 0# 0x7B#Word8 {- { -}
  then EscapeProblem cur (E.BadUnicodeFormat 2#Word64)
  else
    let
      !digitPos = plusAddr# pos 1#
      !(# newPos, code #) = Number.chompHex digitPos end
      !numDigits = minusAddr# newPos digitPos
    in
    if notLtAddr newPos end || neIndex newPos 0# 0x7D#Word8 {- } -} then
      EscapeProblem cur $ E.BadUnicodeFormat (wordToWord64# (int2Word# (2# +# minusAddr# newPos pos)))

    else if code < 0 || 0x10FFFF < code then
      EscapeProblem cur $ E.BadUnicodeCode (wordToWord64# (int2Word# (3# +# minusAddr# newPos pos)))

    else if isTrue# (numDigits <# 4#) || isTrue# (6# <# numDigits) then
      EscapeProblem cur $
        E.BadUnicodeLength (wordToWord64# (int2Word# (3# +# minusAddr# newPos pos))) (I# numDigits) code

    else
      let !(W# code#) = fromIntegral code in
      EscapeUnicode (numDigits +# 4#) code#



-- STRING FSM
--
-- This table is adapted from Bjoern's UTF-8 FSM in C. Notice state 'c' is
-- added for characters that need special attention in somewhat rare cases.
--
-- Copyright (c) 2008-2010 Bjoern Hoehrmann <bjoern@hoehrmann.de>
-- Available under an MIT license, with a great explanation of the code at:
--   http://bjoern.hoehrmann.de/utf-8/decoder/dfa/


string_fsm :: Literals.FSM
string_fsm =
  [Literals.fsm|
    0 0 0 0 0 0 0 0 0 0 c 0 0 c 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 c 0 0 0 0 c 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 c 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9
    7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
    7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
    8 8 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    a 3 3 3 3 3 3 3 3 3 3 3 3 4 3 3
    b 6 6 6 5 8 8 8 8 8 8 8 8 8 8 8
    -------------------------------
    0 1 2 3 5 8 7 1 1 1 4 6 9
    1 1 1 1 1 1 1 1 1 1 1 1 1
    1 0 1 1 1 1 1 0 1 0 1 1 1
    1 2 1 1 1 1 1 2 1 2 1 1 1
    1 1 1 1 1 1 1 2 1 1 1 1 1
    1 2 1 1 1 1 1 1 1 2 1 1 1
    1 1 1 1 1 1 1 3 1 3 1 1 1
    1 3 1 1 1 1 1 3 1 3 1 1 1
    1 3 1 1 1 1 1 1 1 1 1 1 1
  |]



-- CHARACTER HELPERS
--
-- The return Addr# will be the same as the starting Addr# if the char is not
-- properly encoded.


char2 :: Addr# -> Addr# -> Word8# -> (# Addr#, Char #)
char2 pos end word =
  if leAddr pos2 end
  then
    let
      !w1 = word8ToWord# word
      !w2 = getWord8 pos 1#

      !c1 = uncheckedShiftL# (and# 0x3F## w1) 6#
      !c2 = and# 0x3F## w2
    in
    if isTrue# (eqWord8# 0#Word8 (step table w2 (step table w1 0#Word8)))
    then (# pos2, C# (chr# (word2Int# (or# c1 c2))) #)
    else (# pos, placeholder #)
  else
    (# pos, placeholder #)
  where
    !pos2 = plusAddr# pos 2#
    !(Literals.FSM table) = string_fsm


char3 :: Addr# -> Addr# -> Word8# -> (# Addr#, Char #)
char3 pos end word =
  if leAddr pos3 end
  then
    let
      !w1 = word8ToWord# word
      !w2 = getWord8 pos 1#
      !w3 = getWord8 pos 2#

      !c1 = uncheckedShiftL# (and# 0x0F## w1) 12#
      !c2 = uncheckedShiftL# (and# 0x3F## w2) 6#
      !c3 = and# 0x3F## w3
    in
    if isTrue# (eqWord8# 0#Word8 (step table w3 (step table w2 (step table w1 0#Word8))))
    then (# pos3, C# (chr# (word2Int# (or# c1 (or# c2 c3)))) #)
    else (# pos, placeholder #)
  else
    (# pos, placeholder #)
  where
    !pos3 = plusAddr# pos 3#
    !(Literals.FSM table) = string_fsm


char4 :: Addr# -> Addr# -> Word8# -> (# Addr#, Char #)
char4 pos end word =
  if leAddr pos4 end
  then
    let
      !w1 = word8ToWord# word
      !w2 = getWord8 pos 1#
      !w3 = getWord8 pos 2#
      !w4 = getWord8 pos 3#

      !c1 = uncheckedShiftL# (and# 0x07## w1) 18#
      !c2 = uncheckedShiftL# (and# 0x3F## w2) 12#
      !c3 = uncheckedShiftL# (and# 0x3F## w3) 6#
      !c4 = and# 0x3F## w4
    in
    if isTrue# (eqWord8# 0#Word8 (step table w4 (step table w3 (step table w2 (step table w1 0#Word8)))))
    then (# pos4, C# (chr# (word2Int# (or# (or# c1 c2) (or# c3 c4)))) #)
    else (# pos, placeholder #)
  else
    (# pos, placeholder #)
  where
    !pos4 = plusAddr# pos 4#
    !(Literals.FSM table) = string_fsm



-- HELPERS


{-# INLINE getWord8 #-}
getWord8 :: Addr# -> Int# -> Word#
getWord8 addr off =
  word8ToWord# (indexWord8OffAddr# addr off)


{-# INLINE step #-}
step :: Addr# -> Word# -> Word8# -> Word8#
step table word state =
  let
    !tran = indexInt8OffAddr# table (word2Int# word)
  in
  indexWord8OffAddr# table (256# +# word2Int# (word8ToWord# state) +# int8ToInt# tran)

