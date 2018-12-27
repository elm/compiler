{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, UnboxedTuples, OverloadedStrings #-}
module Parse.Symbol
  ( underscore
  , binop
  )
  where


import qualified Data.Char as Char
import qualified Data.IntSet as IntSet
import qualified Data.Name as Name
import qualified Data.Vector as Vector
import Foreign.Ptr (Ptr, plusPtr, minusPtr)
import GHC.Word (Word8)

import Parse.Utils
import qualified Parse.Variable as Var
import qualified Parse.Primitives as P
import qualified Reporting.Error.Syntax as E



-- UNDERSCORE


underscore :: Parser ()
underscore =
  P.Parser $ \(P.State pos end indent row col ctx) cok _ cerr eerr ->
    if pos == end || P.unsafeIndex pos /= 0x5F {- _ -} then
      eerr row col ctx E.Underscore
    else
      let
        !newPos = plusPtr pos 1
        !newCol = col + 1
      in
      if Var.getInnerWidth newPos end > 0 then
        let (# _, badCol #) = Var.chompInnerChars newPos end newCol in
        cerr row newCol ctx (E.UnderscoreNotVar (fromIntegral (badCol - newCol)))
      else
        let !newState = P.State newPos end indent row newCol ctx in
        cok () newState



-- BINOP


binop :: Parser Name.Name
binop =
  P.Parser $ \(P.State pos end indent row col ctx) cok _ cerr eerr ->
    let !newPos = chompOps pos end in
    if pos == newPos then
      eerr row col ctx E.Binop

    else
      case Name.fromPtr pos newPos of
        "."  -> cerr row col ctx (E.ReservedBinop E.Dot)
        "|"  -> cerr row col ctx (E.ReservedBinop E.Pipe)
        "->" -> cerr row col ctx (E.ReservedBinop E.Arrow)
        "="  -> cerr row col ctx (E.ReservedBinop E.Equals)
        ":"  -> cerr row col ctx (E.ReservedBinop E.HasType)
        op   ->
          let
            !newCol = col + fromIntegral (minusPtr newPos pos)
            !newState = P.State newPos end indent row newCol ctx
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
