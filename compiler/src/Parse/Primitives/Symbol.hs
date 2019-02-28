{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, UnboxedTuples, OverloadedStrings #-}
module Parse.Primitives.Symbol
  ( underscore
  , binop
  , equals, rightArrow, hasType, comma, pipe, cons, dot, doubleDot, minus, lambda
  , leftParen, rightParen, leftSquare, rightSquare, leftCurly, rightCurly
  , elmDocCommentOpen, jsMultiCommentOpen, jsMultiCommentClose
  , shaderBlockOpen
  )
  where

import Prelude hiding (length)
import Control.Exception (assert)
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Char as Char
import qualified Data.IntSet as IntSet
import qualified Data.Vector as Vector
import Foreign.Ptr (plusPtr)
import Foreign.ForeignPtr (withForeignPtr, ForeignPtr)
import Foreign.Storable
import GHC.Word (Word8)

import qualified Elm.Name as N
import Parse.Primitives.Internals (Parser(..), State(..), expect, noError)
import qualified Parse.Primitives.Internals as I
import qualified Parse.Primitives.Variable as Var
import qualified Reporting.Error.Syntax as E
import Reporting.Error.Syntax (Theory(..), BadOp(..))



-- UNDERSCORE


underscore :: Parser ()
underscore =
  Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ eerr ->
    if offset == terminal || I.unsafeIndex fp offset /= 0x5F {- _ -} then
      eerr noError
    else
      let
        !newOffset = offset + 1
        !newCol = col + 1
      in
      if Var.getInnerWidth fp newOffset terminal > 0 then
        let (# _, badCol #) = Var.chompInnerChars fp newOffset terminal newCol in
        cerr (E.ParseError row newCol (E.BadUnderscore badCol))
      else
        let !newState = State fp newOffset terminal indent row newCol ctx in
        cok () newState noError



-- BINOP


binop :: Parser N.Name
binop =
  Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ eerr ->
    let !newOffset = chompOps fp offset terminal in
    if offset == newOffset then
      eerr (expect row col ctx E.InfixOp)

    else
      let !length = newOffset - offset in
      case N.fromForeignPtr fp offset length of
        "."  -> cerr (E.ParseError row col (E.BadOp Dot ctx))
        "|"  -> cerr (E.ParseError row col (E.BadOp Pipe ctx))
        "->" -> cerr (E.ParseError row col (E.BadOp Arrow ctx))
        "="  -> cerr (E.ParseError row col (E.BadOp Equals ctx))
        ":"  -> cerr (E.ParseError row col (E.BadOp HasType ctx))
        op   -> cok op (State fp newOffset terminal indent row (col + length) ctx) noError


chompOps :: ForeignPtr Word8 -> Int -> Int -> Int
chompOps fp offset terminal =
  if offset < terminal && isBinopCharHelp (I.unsafeIndex fp offset) then
    chompOps fp (offset + 1) terminal
  else
    offset


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


isOpCharAt ::ForeignPtr Word8 -> Int -> Int -> Int -> Bool
isOpCharAt fp fpOffset fpLength index =
  if index >= fpLength - fpOffset then False
  else B.accursedUnutterablePerformIO $ do
    withForeignPtr fp $ \p -> peek (plusPtr p (fpOffset + index)) >>= pure . isBinopCharHelp


-- PRIVATE SYMBOL IMPLEMENTATION


{- We can some avoid allocation by declaring all available symbols here.
That means the `symbol` function should only be used within this file on
values tagged as NOINLINE.
-}
symbol :: Bool -> B.ByteString -> Parser ()
symbol op sym@(B.PS symFp symOffset symLength) =
  let
    !theory =
      assert
        (I.isNonNewlineAscii symFp symOffset (symOffset + symLength))
        (Symbol (Char8.unpack sym))
  in
  Parser $ \(State fp offset terminal indent row col ctx) cok _ _ eerr ->
    if I.isSubstring symFp symOffset symLength fp offset terminal
       && ( not op
          || not (isOpCharAt fp symOffset terminal (offset + symLength))
          )
    then
      let !newState = State fp (offset + symLength) terminal indent row (col + symLength) ctx in
      cok () newState noError

    else
      eerr (expect row col ctx theory)


-- Operator that cannot be followed by bin op char
operator :: Char8.ByteString -> Parser ()
operator = symbol True


-- Section marker like comment or bracket which will not stick to operators
sectionSymbol :: Char8.ByteString -> Parser ()
sectionSymbol = symbol False


-- COMMON SYMBOLS


{-# NOINLINE equals #-}
equals :: Parser ()
equals =
  operator "="


{-# NOINLINE rightArrow #-}
rightArrow :: Parser ()
rightArrow =
  operator "->"


{-# NOINLINE hasType #-}
hasType :: Parser ()
hasType =
  operator ":"


{-# NOINLINE comma #-}
comma :: Parser ()
comma =
  operator ","


{-# NOINLINE pipe #-}
pipe :: Parser ()
pipe =
  operator "|"


{-# NOINLINE cons #-}
cons :: Parser ()
cons =
  operator "::"


{-# NOINLINE dot #-}
dot :: Parser ()
dot =
  operator "."


{-# NOINLINE doubleDot #-}
doubleDot :: Parser ()
doubleDot =
  operator ".."


{-# NOINLINE minus #-}
minus :: Parser ()
minus =
  operator "-"


{-# NOINLINE lambda #-}
lambda :: Parser ()
lambda =
  operator "\\"



-- ENCLOSURES


{-# NOINLINE leftParen #-}
leftParen :: Parser ()
leftParen =
  sectionSymbol "("


{-# NOINLINE rightParen #-}
rightParen :: Parser ()
rightParen =
  sectionSymbol ")"


{-# NOINLINE leftSquare #-}
leftSquare :: Parser ()
leftSquare =
  sectionSymbol "["


{-# NOINLINE rightSquare #-}
rightSquare :: Parser ()
rightSquare =
  sectionSymbol "]"


{-# NOINLINE leftCurly #-}
leftCurly :: Parser ()
leftCurly =
  sectionSymbol "{"


{-# NOINLINE rightCurly #-}
rightCurly :: Parser ()
rightCurly =
  sectionSymbol "}"



-- COMMENTS


{-# NOINLINE elmDocCommentOpen #-}
elmDocCommentOpen :: Parser ()
elmDocCommentOpen =
  sectionSymbol "{-|"


{-# NOINLINE jsMultiCommentOpen #-}
jsMultiCommentOpen :: Parser ()
jsMultiCommentOpen =
  sectionSymbol "/*"


{-# NOINLINE jsMultiCommentClose #-}
jsMultiCommentClose :: Parser ()
jsMultiCommentClose =
  sectionSymbol "*/"



-- SHADER


{-# NOINLINE shaderBlockOpen #-}
shaderBlockOpen :: Parser ()
shaderBlockOpen =
  sectionSymbol "[glsl|"

