{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, UnboxedTuples #-}
module Parse.Primitives.Number
  ( Number(..)
  , number
  , Outcome(..)
  , chompInt
  , chompHex
  , precedence
  )
  where


import Prelude hiding (length)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Internal as B
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Word (Word8)

import qualified AST.Utils.Binop as Binop
import Parse.Primitives.Internals (Parser(..), State(..), noError)
import qualified Parse.Primitives.Internals as I
import qualified Parse.Primitives.Variable as Var
import qualified Reporting.Error.Syntax as E



-- HELPERS


isDirtyEnd :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Bool
isDirtyEnd fp offset terminal word =
  Var.getInnerWidthHelp fp offset terminal word > 0


{-# INLINE isDecimalDigit #-}
isDecimalDigit :: Word8 -> Bool
isDecimalDigit word =
  word <= 0x39 {- 9 -} && word >= 0x30 {- 0 -}



-- NUMBERS


data Number
  = Int Int
  | Float Double


number :: Parser Number
number =
  Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ eerr ->
    if offset >= terminal then
      eerr noError

    else
      let !word = I.unsafeIndex fp offset in
      if not (isDecimalDigit word) then
        eerr noError

      else
        let
          outcome =
            if word == 0x30 {- 0 -} then
              chompZero fp (offset + 1) terminal
            else
              chompInt fp (offset + 1) terminal (fromIntegral (word - 0x30 {- 0 -}))
        in
          case outcome of
            Err newOffset problem ->
              cerr (E.ParseError row (col + (newOffset - offset)) problem)

            OkInt newOffset n ->
              let
                !integer = Int n
                !newState = State fp newOffset terminal indent row (col + (newOffset - offset)) ctx
              in
              cok integer newState noError

            OkFloat newOffset ->
              let
                !length = newOffset - offset
                !float = Float $ read $ Char8.unpack $ B.PS fp offset length
                !newState = State fp newOffset terminal indent row (col + length) ctx
              in
              cok float newState noError



-- CHOMP OUTCOME


data Outcome
  = Err { _offset :: Int, _problem :: E.Problem }
  | OkInt { _offset :: Int, _value :: Int }
  | OkFloat { _offset :: Int }



-- CHOMP INT


chompInt :: ForeignPtr Word8 -> Int -> Int -> Int -> Outcome
chompInt fp !offset terminal !n =
  if offset >= terminal then

    OkInt offset n

  else

    let
      !word = I.unsafeIndex fp offset
    in
      if isDecimalDigit word then
        chompInt fp (offset + 1) terminal (10 * n + fromIntegral (word - 0x30 {- 0 -}))

      else if word == 0x2E {- . -} then
        chompFraction fp (offset + 1) terminal n

      else if word == 0x65 {- e -} || word == 0x45 {- E -} then
        chompExponent fp (offset + 1) terminal

      else if isDirtyEnd fp offset terminal word then
        Err offset E.BadNumberEnd

      else
        OkInt offset n



-- CHOMP FRACTION


chompFraction :: ForeignPtr Word8 -> Int -> Int -> Int -> Outcome
chompFraction fp offset terminal n =
  if offset >= terminal then
    Err offset (E.BadNumberDot n)

  else if isDecimalDigit (I.unsafeIndex fp offset) then
    chompFractionHelp fp (offset + 1) terminal

  else
    Err offset (E.BadNumberDot n)


chompFractionHelp :: ForeignPtr Word8 -> Int -> Int -> Outcome
chompFractionHelp fp offset terminal =
  if offset >= terminal then
    OkFloat offset

  else
    let !word = I.unsafeIndex fp offset in
    if isDecimalDigit word then
      chompFractionHelp fp (offset + 1) terminal

    else if word == 0x65 {- e -} || word == 0x45 {- E -} then
      chompExponent fp (offset + 1) terminal

    else if isDirtyEnd fp offset terminal word then
      Err offset E.BadNumberEnd

    else
      OkFloat offset



-- CHOMP EXPONENT


chompExponent :: ForeignPtr Word8 -> Int -> Int -> Outcome
chompExponent fp offset terminal =
  if offset >= terminal then
    Err offset E.BadNumberExp

  else
    let !word = I.unsafeIndex fp offset in
    if isDecimalDigit word then
      chompExponentHelp fp (offset + 1) terminal

    else if word == 0x2B {- + -} || word == 0x2D {- - -} then

      let !offset1 = offset + 1 in
      if offset1 < terminal && isDecimalDigit (I.unsafeIndex fp offset1) then
        chompExponentHelp fp (offset + 2) terminal
      else
        Err offset E.BadNumberExp

    else
      Err offset E.BadNumberExp


chompExponentHelp :: ForeignPtr Word8 -> Int -> Int -> Outcome
chompExponentHelp fp offset terminal =
  if offset >= terminal then
    OkFloat offset

  else if isDecimalDigit (I.unsafeIndex fp offset) then
    chompExponentHelp fp (offset + 1) terminal

  else
    OkFloat offset



-- CHOMP ZERO


chompZero :: ForeignPtr Word8 -> Int -> Int -> Outcome
chompZero fp offset terminal =
  if offset >= terminal then
    OkInt offset 0

  else
    let !word = I.unsafeIndex fp offset in
    if word == 0x78 {- x -} then
      chompHexInt fp (offset + 1) terminal

    else if word == 0x2E {- . -} then
      chompFraction fp (offset + 1) terminal 0

    else if isDecimalDigit word then
      Err offset E.BadNumberZero

    else if isDirtyEnd fp offset terminal word then
      Err offset E.BadNumberEnd

    else
      OkInt offset 0


chompHexInt :: ForeignPtr Word8 -> Int -> Int -> Outcome
chompHexInt fp offset terminal =
  let (# newOffset, answer #) = chompHex fp offset terminal in
  if answer < 0 then
    Err newOffset E.BadNumberHex
  else
    OkInt newOffset answer



-- CHOMP HEX


-- Return -1 if it has NO digits
-- Return -2 if it has BAD digits

{-# INLINE chompHex #-}
chompHex :: ForeignPtr Word8 -> Int -> Int -> (# Int, Int #)
chompHex fp offset terminal =
  chompHexHelp fp offset terminal (-1) 0


chompHexHelp :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> (# Int, Int #)
chompHexHelp fp offset terminal answer accumulator =
  if offset >= terminal then
    (# offset, answer #)
  else
    let
      !newAnswer =
        stepHex fp offset terminal (I.unsafeIndex fp offset) accumulator
    in
    if newAnswer < 0 then
      (# offset, if newAnswer == -1 then answer else -2 #)
    else
      chompHexHelp fp (offset + 1) terminal newAnswer newAnswer


{-# INLINE stepHex #-}
stepHex :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Int -> Int
stepHex fp offset terminal word acc
  | 0x30 {- 0 -} <= word && word <= 0x39 {- 9 -} = 16 * acc + fromIntegral (word - 0x30 {- 0 -})
  | 0x61 {- a -} <= word && word <= 0x66 {- f -} = 16 * acc + 10 + fromIntegral (word - 0x61 {- a -})
  | 0x41 {- A -} <= word && word <= 0x46 {- F -} = 16 * acc + 10 + fromIntegral (word - 0x41 {- A -})
  | isDirtyEnd fp offset terminal word           = -2
  | True                                         = -1



-- PRECEDENCE


precedence :: Parser Binop.Precedence
precedence =
  Parser $ \(State fp offset terminal indent row col ctx) cok _ _ eerr ->
    if offset >= terminal then
      eerr noError

    else
      let !word = I.unsafeIndex fp offset in
      if isDecimalDigit word then
        cok
          (Binop.Precedence (fromIntegral (word - 0x30 {- 0 -})))
          (State fp (offset + 1) terminal indent row (col + 1) ctx)
          noError
      else
        eerr noError
