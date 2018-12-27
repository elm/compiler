{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, Rank2Types, UnboxedTuples, OverloadedStrings #-}
module Parse.Primitives
  ( Result(..)
  , fromByteString
  , fromFile
  , Parser(..)
  , State(..)
  , Context(..)
  , oneOf
  , pushContext, popContext, inContext
  , getPosition, getCol, addLocation
  , getIndent, setIndent
  , word1, word2
  , unsafeIndex, isWord, getCharWidth
  )
  where


import Prelude hiding (length)
import qualified Control.Applicative as Applicative (Applicative(..))
import Control.Monad
import qualified Data.ByteString.Internal as B
import Data.Word (Word8, Word16)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek)
import GHC.ForeignPtr (touchForeignPtr, unsafeForeignPtrToPtr)

import qualified File.IO as IO
import qualified Reporting.Annotation as A



-- PARSER


newtype Parser c x a =
  Parser (
    forall b.
      State c
      -> (a -> State c -> b)                        -- consumed ok
      -> (a -> State c -> b)                        -- empty ok
      -> (Word16 -> Word16 -> Context c -> x -> b)  -- consumed err
      -> (Word16 -> Word16 -> Context c -> x -> b)  -- empty err
      -> b
  )


data State c =
  State
    { _pos :: !(Ptr Word8)
    , _end :: !(Ptr Word8)
    , _indent :: !Word16
    , _row :: !Word16
    , _col :: !Word16
    , _context :: Context c
    }


data Context c
  = NoContext
  | Frame {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16 c (Context c)



-- FUNCTOR


instance Functor (Parser c x) where
  {-# INLINE fmap #-}
  fmap f (Parser parser) =
    Parser $ \state cok eok cerr eerr ->
      let
        cok' a s = cok (f a) s
        eok' a s = eok (f a) s
      in
      parser state cok' eok' cerr eerr



-- APPLICATIVE


instance Applicative.Applicative (Parser c x) where
  {-# INLINE pure #-}
  pure = return

  {-# INLINE (<*>) #-}
  (<*>) = ap



-- ONE OF


{-# INLINE oneOf #-}
oneOf :: x -> [Parser c x a] -> Parser c x a
oneOf x parsers =
  Parser $ \state cok eok cerr eerr ->
    oneOfHelp state cok eok cerr eerr x parsers


oneOfHelp
  :: State c
  -> (a -> State c -> b)
  -> (a -> State c -> b)
  -> (Word16 -> Word16 -> Context c -> x -> b)
  -> (Word16 -> Word16 -> Context c -> x -> b)
  -> x
  -> [Parser c x a]
  -> b
oneOfHelp state cok eok cerr eerr x parsers =
  case parsers of
    Parser parser : parsers ->
      let
        eerr' _ _ _ _ =
          oneOfHelp state cok eok cerr eerr x parsers
      in
      parser state cok eok cerr eerr'

    [] ->
      let
        (State _ _ _ row col ctx) = state
      in
      eerr row col ctx x



-- MONAD


instance Monad (Parser c x) where
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


data Result c x a
  = Ok a (State c)
  | Err Word16 Word16 (Context c) x


fromByteString :: Parser c x a -> B.ByteString -> Result c x a
fromByteString (Parser parser) (B.PS fp offset length) =
  B.accursedUnutterablePerformIO $
    let
      !pos = plusPtr (unsafeForeignPtrToPtr fp) offset
      !end = plusPtr pos length
      !result = parser (State pos end 0 1 1 NoContext) Ok Ok Err Err
    in
    do  touchForeignPtr fp
        return result


fromFile :: Parser c x a -> FilePath -> IO (Result c x a)
fromFile parser path =
  fromByteString parser <$> IO.readUtf8 path -- TODO try mmap



-- POSITION


getCol :: Parser c x Word16
getCol =
  Parser $ \state@(State _ _ _ _ col _) _ eok _ _ ->
    eok col state


{-# INLINE getPosition #-}
getPosition :: Parser c x A.Position
getPosition =
  Parser $ \state@(State _ _ _ row col _) _ eok _ _ ->
    eok (A.Position row col) state


addLocation :: Parser c x a -> Parser c x (A.Located a)
addLocation (Parser parser) =
  Parser $ \state@(State _ _ _ srow scol _) cok eok cerr eerr ->
    let
      cok' a s@(State _ _ _ erow ecol _) = cok (A.At (A.Region (A.Position srow scol) (A.Position erow ecol)) a) s
      eok' a s@(State _ _ _ erow ecol _) = eok (A.At (A.Region (A.Position srow scol) (A.Position erow ecol)) a) s
    in
    parser state cok' eok' cerr eerr



-- INDENT


getIndent :: Parser c x Word16
getIndent =
  Parser $ \state@(State _ _ indent _ _ _) _ eok _ _ ->
    eok indent state


setIndent :: Word16 -> Parser c x ()
setIndent indent =
  Parser $ \(State pos end _ row col context) _ eok _ _ ->
    let
      !newState =
        State pos end indent row col context
    in
    eok () newState



-- CONTEXT


pushContext :: A.Position -> c -> Parser c x ()
pushContext (A.Position r c) ctx =
  Parser $ \(State pos end indent row col context) _ eok _ _ ->
    let
      !newContext = Frame r c ctx context
      !newState = State pos end indent row col newContext
    in
    eok () newState


popContext :: a -> Parser c x a
popContext value =
  Parser $ \(State pos end indent row col context) _ eok _ _ ->
    case context of
      Frame _ _ _ context ->
        let
          !newState =
            State pos end indent row col context
        in
        eok value newState

      NoContext ->
        error "compiler error, trying to popContext on NoContext"


inContext :: c -> Parser c x start -> Parser c x a -> Parser c x a
inContext tag (Parser parserStart) (Parser parserA) =
  Parser $ \state@(State _ _ _ frow fcol _) cok eok cerr eerr ->
    let
      cokS _ (State pos end indent row col ctx) =
        let
          !newContext = Frame frow fcol tag ctx
          !newState = State pos end indent row col newContext
          cokA a s = cok a (s { _context = ctx })
        in
        parserA newState cokA cokA cerr cerr

      eokS _ (State pos end indent row col ctx) =
        let
          !newContext = Frame frow fcol tag ctx
          !newState = State pos end indent row col newContext
          cokA a s = cok a (s { _context = ctx })
          eokA a s = eok a (s { _context = ctx })
        in
        parserA newState cokA eokA cerr eerr
    in
    parserStart state cokS eokS cerr eerr



-- SYMBOLS


word1 :: Word8 -> x -> Parser c x ()
word1 word x =
  Parser $ \(State pos end indent row col ctx) cok _ _ eerr ->
    if pos < end && unsafeIndex pos == word then
      let !newState = State (plusPtr pos 1) end indent row (col + 1) ctx in
      cok () newState
    else
      eerr row col ctx x


word2 :: Word8 -> Word8 -> x -> Parser c x ()
word2 w1 w2 x =
  Parser $ \(State pos end indent row col ctx) cok _ _ eerr ->
    let
      !pos1 = plusPtr pos 1
    in
    if pos1 < end && unsafeIndex pos == w1 && unsafeIndex pos1 == w2 then
      let !newState = State (plusPtr pos 2) end indent row (col + 2) ctx in
      cok () newState
    else
      eerr row col ctx x



-- LOW-LEVEL CHECKS


unsafeIndex :: Ptr Word8 -> Word8
unsafeIndex ptr =
  B.accursedUnutterablePerformIO (peek ptr)


{-# INLINE isWord #-}
isWord :: Ptr Word8 -> Ptr Word8 -> Word8 -> Bool
isWord pos end word =
  pos < end && unsafeIndex pos == word


getCharWidth :: Ptr Word8 -> Ptr Word8 -> Word8 -> Int
getCharWidth _pos _end word
  | word < 0x80 = 1
  | word < 0xc0 = error "Need UTF-8 encoded input. Ran into unrecognized bits."
  | word < 0xe0 = 2
  | word < 0xf0 = 3
  | word < 0xf8 = 4
  | True        = error "Need UTF-8 encoded input. Ran into unrecognized bits."
