{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, Rank2Types, UnboxedTuples #-}
module Parse.Primitives
  ( fromByteString
  , Parser(..)
  , State(..)
  , Row
  , Col
  , oneOf, oneOfWithFallback
  , inContext, specialize
  , getPosition, getCol, addLocation, addEnd
  , getIndent, setIndent, withIndent, withBacksetIndent
  , word1, word2
  , unsafeIndex, isWord, getCharWidth
  , Snippet(..)
  , fromSnippet
  )
  where


import Prelude hiding (length)
import qualified Control.Applicative as Applicative (Applicative(..))
import qualified Data.ByteString.Internal as B
import Data.Word (Word8, Word16)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek)
import Foreign.ForeignPtr (ForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)

import qualified Reporting.Annotation as A



-- PARSER


newtype Parser x a =
  Parser (
    forall b.
      State
      -> (a -> State -> b)                       -- consumed ok
      -> (a -> State -> b)                       -- empty ok
      -> (Row -> Col -> (Row -> Col -> x) -> b)  -- consumed err
      -> (Row -> Col -> (Row -> Col -> x) -> b)  -- empty err
      -> b
  )


data State = -- PERF try taking some out to avoid allocation
  State
    { _src :: ForeignPtr Word8
    , _pos :: !(Ptr Word8)
    , _end :: !(Ptr Word8)
    , _indent :: !Word16
    , _row :: !Row
    , _col :: !Col
    }


type Row = Word16
type Col = Word16



-- FUNCTOR


instance Functor (Parser x) where
  {-# INLINE fmap #-}
  fmap f (Parser parser) =
    Parser $ \state cok eok cerr eerr ->
      let
        cok' a s = cok (f a) s
        eok' a s = eok (f a) s
      in
      parser state cok' eok' cerr eerr



-- APPLICATIVE


instance Applicative.Applicative (Parser x) where
  {-# INLINE pure #-}
  pure = return

  {-# INLINE (<*>) #-}
  (<*>) (Parser parserFunc) (Parser parserArg) =
    Parser $ \state cok eok cerr eerr ->
      let
        cokF func s1 =
          let
            cokA arg s2 = cok (func arg) s2
          in
          parserArg s1 cokA cokA cerr cerr

        eokF func s1 =
          let
            cokA arg s2 = cok (func arg) s2
            eokA arg s2 = eok (func arg) s2
          in
          parserArg s1 cokA eokA cerr eerr
      in
      parserFunc state cokF eokF cerr eerr



-- ONE OF


{-# INLINE oneOf #-}
oneOf :: (Row -> Col -> x) -> [Parser x a] -> Parser x a
oneOf toError parsers =
  Parser $ \state cok eok cerr eerr ->
    oneOfHelp state cok eok cerr eerr toError parsers


oneOfHelp
  :: State
  -> (a -> State -> b)
  -> (a -> State -> b)
  -> (Row -> Col -> (Row -> Col -> x) -> b)
  -> (Row -> Col -> (Row -> Col -> x) -> b)
  -> (Row -> Col -> x)
  -> [Parser x a]
  -> b
oneOfHelp state cok eok cerr eerr toError parsers =
  case parsers of
    Parser parser : parsers ->
      let
        eerr' _ _ _ =
          oneOfHelp state cok eok cerr eerr toError parsers
      in
      parser state cok eok cerr eerr'

    [] ->
      let
        (State _ _ _ _ row col) = state
      in
      eerr row col toError



-- ONE OF WITH FALLBACK


{-# INLINE oneOfWithFallback #-}
oneOfWithFallback :: [Parser x a] -> a -> Parser x a -- PERF is this function okay? Worried about allocation/laziness with fallback values.
oneOfWithFallback parsers fallback =
  Parser $ \state cok eok cerr _ ->
    oowfHelp state cok eok cerr parsers fallback


oowfHelp
  :: State
  -> (a -> State -> b)
  -> (a -> State -> b)
  -> (Row -> Col -> (Row -> Col -> x) -> b)
  -> [Parser x a]
  -> a
  -> b
oowfHelp state cok eok cerr parsers fallback =
  case parsers of
    [] ->
      eok fallback state

    Parser parser : parsers ->
      let
        eerr' _ _ _ =
          oowfHelp state cok eok cerr parsers fallback
      in
      parser state cok eok cerr eerr'



-- MONAD


instance Monad (Parser x) where
  {-# INLINE return #-}
  return value =
    Parser $ \state _ eok _ _ ->
      eok value state

  {-# INLINE (>>=) #-}
  (Parser parserA) >>= callback =
    Parser $ \state cok eok cerr eerr ->
      let
        cok' a s =
          case callback a of
            Parser parserB -> parserB s cok cok cerr cerr

        eok' a s =
          case callback a of
            Parser parserB -> parserB s cok eok cerr eerr
      in
      parserA state cok' eok' cerr eerr



-- FROM BYTESTRING


fromByteString :: Parser x a -> (Row -> Col -> x) -> B.ByteString -> Either x a
fromByteString (Parser parser) toBadEnd (B.PS fptr offset length) =
  B.accursedUnutterablePerformIO $
    let
      toOk' = toOk toBadEnd
      !pos = plusPtr (unsafeForeignPtrToPtr fptr) offset
      !end = plusPtr pos length
      !result = parser (State fptr pos end 0 1 1) toOk' toOk' toErr toErr
    in
    do  touchForeignPtr fptr
        return result


toOk :: (Row -> Col -> x) -> a -> State -> Either x a
toOk toBadEnd !a (State _ pos end _ row col) =
  if pos == end
  then Right a
  else Left (toBadEnd row col)


toErr :: Row -> Col -> (Row -> Col -> x) -> Either x a
toErr row col toError =
  Left (toError row col)



-- FROM SNIPPET


data Snippet =
  Snippet
    { _fptr   :: ForeignPtr Word8
    , _offset :: Int
    , _length :: Int
    , _offRow :: Row
    , _offCol :: Col
    }


fromSnippet :: Parser x a -> (Row -> Col -> x) -> Snippet -> Either x a
fromSnippet (Parser parser) toBadEnd (Snippet fptr offset length row col) =
  B.accursedUnutterablePerformIO $
    let
      toOk' = toOk toBadEnd
      !pos = plusPtr (unsafeForeignPtrToPtr fptr) offset
      !end = plusPtr pos length
      !result = parser (State fptr pos end 0 row col) toOk' toOk' toErr toErr
    in
    do  touchForeignPtr fptr
        return result



-- POSITION


getCol :: Parser x Word16
getCol =
  Parser $ \state@(State _ _ _ _ _ col) _ eok _ _ ->
    eok col state


{-# INLINE getPosition #-}
getPosition :: Parser x A.Position
getPosition =
  Parser $ \state@(State _ _ _ _ row col) _ eok _ _ ->
    eok (A.Position row col) state


addLocation :: Parser x a -> Parser x (A.Located a)
addLocation (Parser parser) =
  Parser $ \state@(State _ _ _ _ sr sc) cok eok cerr eerr ->
    let
      cok' a s@(State _ _ _ _ er ec) = cok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) a) s
      eok' a s@(State _ _ _ _ er ec) = eok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) a) s
    in
    parser state cok' eok' cerr eerr


addEnd :: A.Position -> a -> Parser x (A.Located a)
addEnd start value =
  Parser $ \state@(State _ _ _ _ row col) _ eok _ _ ->
    eok (A.at start (A.Position row col) value) state



-- INDENT


getIndent :: Parser x Word16
getIndent =
  Parser $ \state@(State _ _ _ indent _ _) _ eok _ _ ->
    eok indent state


setIndent :: Word16 -> Parser x ()
setIndent indent =
  Parser $ \(State src pos end _ row col) _ eok _ _ ->
    let
      !newState = State src pos end indent row col
    in
    eok () newState


withIndent :: Parser x a -> Parser x a
withIndent (Parser parser) =
  Parser $ \(State src pos end oldIndent row col) cok eok cerr eerr ->
    let
      cok' a (State s p e _ r c) = cok a (State s p e oldIndent r c)
      eok' a (State s p e _ r c) = eok a (State s p e oldIndent r c)
    in
    parser (State src pos end col row col) cok' eok' cerr eerr


withBacksetIndent :: Word16 -> Parser x a -> Parser x a
withBacksetIndent backset (Parser parser) =
  Parser $ \(State src pos end oldIndent row col) cok eok cerr eerr ->
    let
      cok' a (State s p e _ r c) = cok a (State s p e oldIndent r c)
      eok' a (State s p e _ r c) = eok a (State s p e oldIndent r c)
    in
    parser (State src pos end (col - backset) row col) cok' eok' cerr eerr



-- CONTEXT


inContext :: (x -> Row -> Col -> y) -> Parser y start -> Parser x a -> Parser y a
inContext addContext (Parser parserStart) (Parser parserA) =
  Parser $ \state@(State _ _ _ _ row col) cok eok cerr eerr ->
    let
      cerrA r c tx = cerr row col (addContext (tx r c))
      eerrA r c tx = eerr row col (addContext (tx r c))

      cokS _ s = parserA s cok cok cerrA cerrA
      eokS _ s = parserA s cok eok cerrA eerrA
    in
    parserStart state cokS eokS cerr eerr


specialize :: (x -> Row -> Col -> y) -> Parser x a -> Parser y a
specialize addContext (Parser parser) =
  Parser $ \state@(State _ _ _ _ row col) cok eok cerr eerr ->
    let
      cerr' r c tx = cerr row col (addContext (tx r c))
      eerr' r c tx = eerr row col (addContext (tx r c))
    in
    parser state cok eok cerr' eerr'



-- SYMBOLS


word1 :: Word8 -> (Row -> Col -> x) -> Parser x ()
word1 word toError =
  Parser $ \(State src pos end indent row col) cok _ _ eerr ->
    if pos < end && unsafeIndex pos == word then
      let !newState = State src (plusPtr pos 1) end indent row (col + 1) in
      cok () newState
    else
      eerr row col toError


word2 :: Word8 -> Word8 -> (Row -> Col -> x) -> Parser x ()
word2 w1 w2 toError =
  Parser $ \(State src pos end indent row col) cok _ _ eerr ->
    let
      !pos1 = plusPtr pos 1
    in
    if pos1 < end && unsafeIndex pos == w1 && unsafeIndex pos1 == w2 then
      let !newState = State src (plusPtr pos 2) end indent row (col + 2) in
      cok () newState
    else
      eerr row col toError



-- LOW-LEVEL CHECKS


unsafeIndex :: Ptr Word8 -> Word8
unsafeIndex ptr =
  B.accursedUnutterablePerformIO (peek ptr)


{-# INLINE isWord #-}
isWord :: Ptr Word8 -> Ptr Word8 -> Word8 -> Bool
isWord pos end word =
  pos < end && unsafeIndex pos == word


getCharWidth :: Word8 -> Int
getCharWidth word
  | word < 0x80 = 1
  | word < 0xc0 = error "Need UTF-8 encoded input. Ran into unrecognized bits."
  | word < 0xe0 = 2
  | word < 0xf0 = 3
  | word < 0xf8 = 4
  | True        = error "Need UTF-8 encoded input. Ran into unrecognized bits."
