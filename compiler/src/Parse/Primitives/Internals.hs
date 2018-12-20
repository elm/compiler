{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE BangPatterns, Rank2Types, UnboxedTuples, OverloadedStrings #-}
module Parse.Primitives.Internals
  ( Parser(..)
  , State(..)
  , noError, expect
  , unsafeIndex, isWord, getCharWidth
  , isSubstring, isNonNewlineAscii
  , oneOf
  )
  where


import Prelude hiding (length)
import qualified Control.Applicative as Applicative (Applicative(..))
import Control.Monad
import qualified Data.ByteString.Internal as B
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.ForeignPtr (touchForeignPtr, unsafeForeignPtrToPtr)
import GHC.Word (Word8)

import qualified Reporting.Error.Syntax as E



-- PARSER


newtype Parser a =
  Parser (
    forall b.
      State
      -> (a -> State -> E.ParseError -> b)  -- consumed ok
      -> (              E.ParseError -> b)  -- consumed err
      -> (a -> State -> E.ParseError -> b)  -- empty ok
      -> (              E.ParseError -> b)  -- empty err
      -> b
  )


data State =
  State
    { _source :: !(ForeignPtr Word8)
    , _offset :: !Int
    , _terminal :: !Int
    , _indent :: !Int
    , _row :: !Int
    , _col :: !Int
    , _context :: E.ContextStack
    }



-- ERRORS


{-# NOINLINE noError #-}
noError :: E.ParseError
noError =
  E.ParseError 0 0 (E.Theories [] [])


{-# INLINE expect #-}
expect :: Int -> Int -> E.ContextStack -> E.Theory -> E.ParseError
expect row col ctx theory =
  E.ParseError row col (E.Theories ctx [theory])



-- LOW-LEVEL CHECKS


unsafeIndex :: ForeignPtr Word8 -> Int -> Word8
unsafeIndex fp offset =
  B.accursedUnutterablePerformIO $
    do  word <- peekByteOff (unsafeForeignPtrToPtr fp) offset
        touchForeignPtr fp
        return word


{-# INLINE isWord #-}
isWord :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Bool
isWord fp offset terminal word =
  offset < terminal && unsafeIndex fp offset == word


getCharWidth :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Int
getCharWidth _fp _offset _terminal word
  | word < 0x80 = 1
  | word < 0xc0 = error "Need UTF-8 encoded input. Ran into unrecognized bits."
  | word < 0xe0 = 2
  | word < 0xf0 = 3
  | word < 0xf8 = 4
  | True        = error "Need UTF-8 encoded input. Ran into unrecognized bits."


{-# INLINE isSubstring #-}
isSubstring :: ForeignPtr Word8 -> Int -> Int -> ForeignPtr Word8 -> Int -> Int -> Bool
isSubstring subFp subOffset subLength fp offset terminal =
  offset + subLength <= terminal
  &&
  equals fp offset subFp subOffset subLength


equals :: ForeignPtr Word8 -> Int -> ForeignPtr Word8 -> Int -> Int -> Bool
equals fp1 offset1 fp2 offset2 length =
  B.accursedUnutterablePerformIO $
    withForeignPtr fp1 $ \ptr1 ->
    withForeignPtr fp2 $ \ptr2 ->
      do  i <- B.memcmp (plusPtr ptr1 offset1) (plusPtr ptr2 offset2) (fromIntegral length)
          return $! i == 0



-- VERIFY BYTESTRING STRUCTURE


-- Written weird to try to make TCE more likely
isNonNewlineAscii :: ForeignPtr Word8 -> Int -> Int -> Bool
isNonNewlineAscii !fp !offset !terminal =
  if offset < terminal then
    let !word = unsafeIndex fp offset in
    if word < 128 && word /= 0x0A {- \n -} then
      isNonNewlineAscii fp (offset + 1) terminal
    else
      False

  else
    True



-- FUNCTOR


instance Functor Parser where
  {-# INLINE fmap #-}
  fmap f (Parser parser) =
    Parser $ \state cok cerr eok eerr ->
      let
        cok' x s e = cok (f x) s e
        eok' x s e = eok (f x) s e
      in
        parser state cok' cerr eok' eerr



-- APPLICATIVE


instance Applicative.Applicative Parser where
    {-# INLINE pure #-}
    pure = return

    {-# INLINE (<*>) #-}
    (<*>) = ap


oneOf :: [Parser a] -> Parser a
oneOf parsers =
  foldr oneOfHelp allTheOptionsFailed parsers


allTheOptionsFailed :: Parser a
allTheOptionsFailed =
  Parser $ \_ _ _ _ eerr ->
    eerr noError


oneOfHelp :: Parser a -> Parser a -> Parser a
oneOfHelp (Parser parser1) (Parser parser2) =
  Parser $ \state cok cerr eok eerr ->
    let
      eerr1 e1 =
        let
          eok2 y s e2 = eok y s (mergeErrors e1 e2)
          eerr2 e2 = eerr (mergeErrors e1 e2)
        in
          parser2 state cok cerr eok2 eerr2
    in
      parser1 state cok cerr eok eerr1


mergeErrors :: E.ParseError -> E.ParseError -> E.ParseError
mergeErrors e1@(E.ParseError r1 c1 p1) e2@(E.ParseError r2 c2 p2) =
  case compare r1 r2 of
    LT -> e2
    GT -> e1
    EQ ->
      case compare c1 c2 of
        LT -> e2
        GT -> e1
        EQ ->
          case (p1, p2) of
            (E.Theories _ [], E.Theories _ _) ->
              e2

            (E.Theories _ _, E.Theories _ []) ->
              e1

            (E.Theories ctx ts1, E.Theories _ ts2) ->
              E.ParseError r1 c1 (E.Theories ctx (ts1 ++ ts2))

            (E.Theories _ _, _) ->
              e2

            (_, _) ->
              e1



-- MONAD


instance Monad Parser where
  {-# INLINE return #-}
  return value =
    Parser $ \state _ _ eok _ ->
      eok value state noError

  {-# INLINE (>>=) #-}
  (Parser parser) >>= callback =
    Parser $ \state cok cerr eok eerr ->
      let
        cok1 x s1 e1 =
          let
            eok2 y s2 e2 = cok y s2 (mergeErrors e1 e2)
            eerr2 e2 = cerr (mergeErrors e1 e2)
          in
          case callback x of
            Parser parser2 -> parser2 s1 cok cerr eok2 eerr2

        eok1 x s1 e1 =
          let
            eok2 y s2 e2 = eok y s2 (mergeErrors e1 e2)
            eerr2 e2 = eerr (mergeErrors e1 e2)
          in
          case callback x of
            Parser parser2 -> parser2 s1 cok cerr eok2 eerr2
      in
        parser state cok1 cerr eok1 eerr
