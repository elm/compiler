{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, UnboxedTuples, UnliftedDatatypes #-}
module Parse.Number
  ( Number(..)
  , number
  , Outcome(..)
  , chompInt
  , chompHex
  )
  where


import GHC.Base (UnliftedType)
import GHC.Exts (isTrue#)
import GHC.Prim
import GHC.Word (Word8(..))

import qualified AST.Prim.Variable as Var
import qualified Elm.Float as EF
import Parse.Primitives (Parser, Cursor, ltAddr, notLtAddr, slide)
import qualified Parse.Primitives as P
import qualified Reporting.Error.Syntax as E



-- NUMBERS


data Number
  = Int Integer
  | Float EF.Float


number :: (Cursor -> x) -> (E.Number -> Cursor -> x) -> Parser x Number
number toExpectation toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ cerr eerr ->
    if notLtAddr pos end then
      eerr cur toExpectation

    else
      let !w = indexWord8OffAddr# pos 0# in
      if not (isDecDigit w) then
        eerr cur toExpectation

      else
        let
          outcome =
            case w of
              0x30#Word8 {-0-} -> chompZero (plusAddr# pos 1#) end
              _                -> chompInt  (plusAddr# pos 1#) end (fromDecDigit w)
        in
        case outcome of
          Err newPos problem ->
            do  let !newCur = slide cur (wordToWord64# (int2Word# (minusAddr# newPos pos)))
                cerr newCur (toError problem)

          OkInt newPos n ->
            do  let !newCur = slide cur (wordToWord64# (int2Word# (minusAddr# newPos pos)))
                let !integer = Int n
                let !newState = P.State newPos end indent newCur
                cok integer newState

          OkFloat newPos ->
            do  let !newCur = slide cur (wordToWord64# (int2Word# (minusAddr# newPos pos)))
                let !newState = P.State newPos end indent newCur
                float <- EF.fromAddr pos newPos
                cok (Float float) newState



-- CHOMP OUTCOME


-- first Int is newPos
--
type Outcome :: UnliftedType
data Outcome
  = Err Addr# E.Number
  | OkInt Addr# Integer
  | OkFloat Addr#



-- CHOMP INT


chompInt :: Addr# -> Addr# -> Integer -> Outcome
chompInt pos end !n =
  if notLtAddr pos end
  then OkInt pos n
  else
    case indexWord8OffAddr# pos 0# of
      w | isDecDigit w     -> chompInt (plusAddr# pos 1#) end (10 * n + fromDecDigit w)
      0x2E#Word8 {-.-}     -> chompFraction pos end n
      0x65#Word8 {-e-}     -> chompExponent (plusAddr# pos 1#) end
      0x45#Word8 {-E-}     -> chompExponent (plusAddr# pos 1#) end
      w | badEnd pos end w -> Err pos E.NumberEnd
      _                    -> OkInt pos n



-- CHOMP FRACTION


chompFraction :: Addr# -> Addr# -> Integer -> Outcome
chompFraction pos end n =
  let
    !pos1 = plusAddr# pos 1#
  in
  if ltAddr pos1 end && isDecDigit (indexWord8OffAddr# pos1 0#)
  then chompFractionHelp (plusAddr# pos1 1#) end
  else Err pos (E.NumberDot n)


chompFractionHelp :: Addr# -> Addr# -> Outcome
chompFractionHelp pos end =
  if notLtAddr pos end
  then OkFloat pos
  else
    case indexWord8OffAddr# pos 0# of
      w | isDecDigit w     -> chompFractionHelp (plusAddr# pos 1#) end
      0x65#Word8 {-e-}     -> chompExponent (plusAddr# pos 1#) end
      0x45#Word8 {-E-}     -> chompExponent (plusAddr# pos 1#) end
      w | badEnd pos end w -> Err pos E.NumberEnd
      _                    -> OkFloat pos



-- CHOMP EXPONENT


chompExponent :: Addr# -> Addr# -> Outcome
chompExponent pos end =
  if notLtAddr pos end
  then Err pos E.NumberEnd
  else
    case indexWord8OffAddr# pos 0# of
      w | isDecDigit w -> chompExponentHelp (plusAddr# pos 1#) end
      0x2B#Word8 {-+-} -> chompExponentSigned pos end
      0x2D#Word8 {---} -> chompExponentSigned pos end
      _                -> Err pos E.NumberEnd


chompExponentSigned :: Addr# -> Addr# -> Outcome
chompExponentSigned pos end =
  let !pos1 = plusAddr# pos 1# in
  if ltAddr pos1 end && isDecDigit (indexWord8OffAddr# pos1 0#)
  then chompExponentHelp (plusAddr# pos 2#) end
  else Err pos E.NumberEnd


chompExponentHelp :: Addr# -> Addr# -> Outcome
chompExponentHelp pos end =
  if ltAddr pos end && isDecDigit (indexWord8OffAddr# pos 0#)
  then chompExponentHelp (plusAddr# pos 1#) end
  else OkFloat pos



-- CHOMP ZERO


chompZero :: Addr# -> Addr# -> Outcome
chompZero pos end =
  if notLtAddr pos end
  then OkInt pos 0
  else
    case indexWord8OffAddr# pos 0# of
      0x78#Word8 {-x-}     -> chompHexInt (plusAddr# pos 1#) end
      0x2E#Word8 {-.-}     -> chompFraction pos end 0
      w | isDecDigit w     -> Err pos E.NumberNoLeadingZero
        | badEnd pos end w -> Err pos E.NumberEnd
        | otherwise        -> OkInt pos 0


chompHexInt :: Addr# -> Addr# -> Outcome
chompHexInt pos end =
  let !(# newPos, answer #) = chompHex pos end in
  if answer < 0
  then Err newPos E.NumberHexDigit
  else OkInt newPos answer



-- CHOMP HEX


-- Return -1 if it has NO digits
-- Return -2 if it has BAD digits

{-# INLINE chompHex #-}
chompHex :: Addr# -> Addr# -> (# Addr#, Integer #)
chompHex pos end =
  chompHexHelp pos end (-1) 0


chompHexHelp :: Addr# -> Addr# -> Integer -> Integer -> (# Addr#, Integer #)
chompHexHelp pos end answer accumulator =
  if notLtAddr pos end then
    (# pos, answer #)
  else
    let
      !newAnswer =
        stepHex pos end (indexWord8OffAddr# pos 0#) accumulator
    in
    if newAnswer < 0 then
      (# pos, if newAnswer == -1 then answer else -2 #)
    else
      chompHexHelp (plusAddr# pos 1#) end newAnswer newAnswer


{-# INLINE stepHex #-}
stepHex :: Addr# -> Addr# -> Word8# -> Integer -> Integer
stepHex pos end w acc
  | between 0x30#Word8 w 0x39#Word8 {-0-9-} = 16 * acc + fromDecDigit w
  | between 0x61#Word8 w 0x66#Word8 {-a-f-} = 16 * acc + fromLowerHex w
  | between 0x41#Word8 w 0x46#Word8 {-A-F-} = 16 * acc + fromUpperHex w
  | badEnd pos end w                        = -2
  | True                                    = -1



-- HELPERS


badEnd :: Addr# -> Addr# -> Word8# -> Bool
badEnd pos end w =
  ltAddr pos (Var.chompInner pos end w)


{-# INLINE isDecDigit #-}
isDecDigit :: Word8# -> Bool
isDecDigit word =
  between 0x30#Word8 word 0x39#Word8 {-0-} {-9-}


{-# INLINE between #-}
between :: Word8# -> Word8# -> Word8# -> Bool
between lo word hi =
  isTrue# (leWord8# word hi)
  &&
  isTrue# (geWord8# word lo)


{-# INLINE fromDecDigit #-}
{-# INLINE fromUpperHex #-}
{-# INLINE fromLowerHex #-}

fromDecDigit :: Word8# -> Integer
fromUpperHex :: Word8# -> Integer
fromLowerHex :: Word8# -> Integer

fromDecDigit w = fromIntegral (     W8# w - 0x30 {-0-})
fromUpperHex w = fromIntegral (10 + W8# w - 0x41 {-A-})
fromLowerHex w = fromIntegral (10 + W8# w - 0x61 {-a-})
