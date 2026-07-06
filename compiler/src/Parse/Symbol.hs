{-# LANGUAGE BangPatterns, BinaryLiterals, ExtendedLiterals, MagicHash #-}
module Parse.Symbol
  ( BadOperator(..)
  , operator
  --
  , binopCharSet
  )
  where


import qualified Data.Char as Char
import qualified Data.IntSet as IntSet
import qualified Data.Name as Name
import GHC.Exts (isTrue#)
import GHC.Prim

import Parse.Primitives (Parser, Cursor, ltAddr, eqIndex, slide)
import qualified Parse.Primitives as P



-- OPERATOR


data BadOperator
  = BadDot
  | BadPipe
  | BadArrow
  | BadEquals
  | BadHasType


operator :: (Cursor -> x) -> (BadOperator -> Cursor -> x) -> Parser x Name.Name
operator toExpectation toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ cerr eerr ->
    if ltAddr pos end
    then
      let
        !w = indexWord8OffAddr# pos 0#
      in
      if isSymbol w
      then
        let
          !newPos = chompOps (plusAddr# pos 1#) end
        in
        case minusAddr# newPos pos of
          1# ->
            case w of
              0x2E#Word8 {-.-} -> eerr cur (toError BadDot)
              0x7C#Word8 {-|-} -> cerr cur (toError BadPipe)
              0x3D#Word8 {-=-} -> cerr cur (toError BadEquals)
              0x3A#Word8 {-:-} -> cerr cur (toError BadHasType)
              _ ->
                do  let !newState = P.State newPos end indent (slide cur 1#Word64)
                    op <- Name.fromAddr pos newPos
                    cok op newState

          2#
            | isTrue# (eqWord8# w 0x2D#Word8) {---} && eqIndex pos 1# 0x3E#Word8 {->-} -> cerr cur (toError BadArrow)

          width ->
            do  let !newState = P.State newPos end indent (slide cur (wordToWord64# (int2Word# width)))
                op <- Name.fromAddr pos newPos
                cok op newState
      else
        eerr cur toExpectation
    else
      eerr cur toExpectation


chompOps :: Addr# -> Addr# -> Addr#
chompOps pos end =
  if ltAddr pos end && isSymbol (indexWord8OffAddr# pos 0#)
  then chompOps (plusAddr# pos 1#) end
  else pos



-- IS SYMBOL
--
-- PERF is it faster to encode as an Addr# and skip the branching?


{-# INLINE isSymbol #-}
isSymbol :: Word8# -> Bool
isSymbol w
  | isTrue# (i <#  64#) = isTrue# (neWord64# 0#Word64 (and64# (uncheckedShiftL64# 1#Word64 (i       )) 0b1111010000000000111011000110001000000000000000000000000000000000#Word64))
  | isTrue# (i <# 128#) = isTrue# (neWord64# 0#Word64 (and64# (uncheckedShiftL64# 1#Word64 (i -# 64#)) 0b0001000000000000000000000000000001000000000000000000000000000000#Word64))
  | otherwise           = False
  where
    !i = word2Int# (word8ToWord# w)


{-# NOINLINE binopCharSet #-}
binopCharSet :: IntSet.IntSet
binopCharSet =
  IntSet.fromList (map Char.ord "+-/*=.<>:&|^?%!")



