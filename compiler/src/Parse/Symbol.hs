{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, UnboxedTuples, OverloadedStrings #-}
module Parse.Symbol
  ( wildcard
  , operator
  )
  where


import qualified Data.Char as Char
import qualified Data.IntSet as IntSet
import qualified Data.Name as Name
import qualified Data.Vector as Vector
import Foreign.Ptr (Ptr, plusPtr, minusPtr)
import GHC.Word (Word8)

import Parse.Primitives (Parser, Row, Col)
import qualified Parse.Variable as Var
import qualified Parse.Primitives as P
import qualified Reporting.Error.Syntax as E



-- UNDERSCORE


wildcard :: Parser E.Pattern ()
wildcard =
  P.Parser $ \(P.State pos end indent row col) cok _ cerr eerr ->
    if pos == end || P.unsafeIndex pos /= 0x5F {- _ -} then
      eerr row col E.PStart
    else
      let
        !newPos = plusPtr pos 1
        !newCol = col + 1
      in
      if Var.getInnerWidth newPos end > 0 then
        let (# _, badCol #) = Var.chompInnerChars newPos end newCol in
        cerr row col (E.PWildcardNotVar (fromIntegral (badCol - col)))
      else
        let !newState = P.State newPos end indent row newCol in
        cok () newState



-- OPERATOR


operator :: (Row -> Col -> x) -> (E.Operator -> Row -> Col -> x) -> Parser x Name.Name
operator toExpectation toError =
  P.Parser $ \(P.State pos end indent row col) cok _ cerr eerr ->
    let !newPos = chompOps pos end in
    if pos == newPos then
      eerr row col toExpectation

    else
      case Name.fromPtr pos newPos of
        "."  -> cerr row col (toError E.OpDot)
        "|"  -> cerr row col (toError E.OpPipe)
        "->" -> cerr row col (toError E.OpArrow)
        "="  -> cerr row col (toError E.OpEquals)
        ":"  -> cerr row col (toError E.OpHasType)
        op   ->
          let
            !newCol = col + fromIntegral (minusPtr newPos pos)
            !newState = P.State newPos end indent row newCol
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
