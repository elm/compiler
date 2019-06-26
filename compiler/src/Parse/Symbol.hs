{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Parse.Symbol
  ( operator
  , BadOperator(..)
  , binopCharSet
  )
  where


import qualified Data.Char as Char
import qualified Data.IntSet as IntSet
import qualified Data.Name as Name
import qualified Data.Vector as Vector
import Foreign.Ptr (Ptr, plusPtr, minusPtr)
import GHC.Word (Word8)

import Parse.Primitives (Parser, Row, Col)
import qualified Parse.Primitives as P



-- OPERATOR


data BadOperator
  = BadDot
  | BadPipe
  | BadArrow
  | BadEquals
  | BadHasType


operator :: (Row -> Col -> x) -> (BadOperator -> Row -> Col -> x) -> Parser x Name.Name
operator toExpectation toError =
  P.Parser $ \(P.State src pos end indent row col) cok _ cerr eerr ->
    let !newPos = chompOps pos end in
    if pos == newPos then
      eerr row col toExpectation

    else
      case Name.fromPtr pos newPos of
        "."  -> eerr row col (toError BadDot)
        "|"  -> cerr row col (toError BadPipe)
        "->" -> cerr row col (toError BadArrow)
        "="  -> cerr row col (toError BadEquals)
        ":"  -> cerr row col (toError BadHasType)
        op   ->
          let
            !newCol = col + fromIntegral (minusPtr newPos pos)
            !newState = P.State src newPos end indent row newCol
          in
          cok op newState


chompOps :: Ptr Word8 -> Ptr Word8 -> Ptr Word8
chompOps pos end =
  if pos < end && isBinopCharHelp (P.unsafeIndex pos) then
    chompOps (plusPtr pos 1) end
  else
    pos


{-# INLINE isBinopCharHelp #-}
isBinopCharHelp :: Word8 -> Bool
isBinopCharHelp word =
  word < 128 && Vector.unsafeIndex binopCharVector (fromIntegral word)


{-# NOINLINE binopCharVector #-}
binopCharVector :: Vector.Vector Bool
binopCharVector =
  Vector.generate 128 (\i -> IntSet.member i binopCharSet)


{-# NOINLINE binopCharSet #-}
binopCharSet :: IntSet.IntSet
binopCharSet =
  IntSet.fromList (map Char.ord "+-/*=.<>:&|^?%!")
