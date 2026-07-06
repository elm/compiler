{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, Rank2Types, UnboxedTuples,
UnliftedDatatypes
#-}
module Parse.Primitives
  ( fromByteString
  , Parser(..)
  , State(..)
  , Indent
  --
  , Cursor
  , slide
  , newline
  , isLineStart
  , isAligned
  , isIndented
  , distanceToIndent
  --
  , oneOf, oneOfWithFallback
  , inContext, specialize
  , getPosition, getRegion, addLocation, addEnd
  , withIndent, withBacksetIndent
  , word1, word2
  , Snippet(..)
  , fromSnippet
  --
  , eqAddr, ltAddr, leAddr, notLtAddr, ltAddrOff
  , eqIndex, neIndex
  --
  , skipUtf8
  )
  where


import qualified Control.Applicative as Applicative (Applicative(..))
import qualified Data.ByteString.Internal as B
import GHC.Base (UnliftedType)
import GHC.Exts (isTrue#)
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents)
import GHC.Int (Int(..))
import GHC.Prim
import GHC.Word (Word32(..))

import qualified Bytes
import qualified Literals

import qualified Reporting.Annotation as A



-- PARSER


newtype Parser x a =
  Parser (
    forall b.
      ForeignPtrContents
      -> State
      -> (a -> State -> IO b)               -- consumed ok
      -> (a -> State -> IO b)               -- empty ok
      -> (Cursor -> (Cursor -> x) -> IO b)  -- consumed err
      -> (Cursor -> (Cursor -> x) -> IO b)  -- empty err
      -> IO b
  )


type State :: UnliftedType
data State =
  State
    { _pos :: Addr#
    , _end :: Addr#
    , _indent :: Indent
    , _cursor :: Cursor
    }


type Indent = Word32#



-- CURSOR


type Cursor = Word64#


{-# INLINE slide #-}
slide :: Cursor -> Word64# -> Cursor
slide cur off =
  plusWord64# cur off


{-# INLINE newline #-}
newline :: Cursor -> Cursor
newline cur =
  and64#
    (plusWord64# cur 0x100000000#Word64)
    0xFFFFFFFF00000000#Word64


{-# INLINE isLineStart #-}
isLineStart :: Cursor -> Bool
isLineStart cur =
  isTrue# (eqWord# 0## (narrow32Word# (word64ToWord# cur)))


{-# INLINE isAligned #-}
isAligned :: Indent -> Cursor -> Bool
isAligned indent cur =
  isTrue# (eqWord32# (wordToWord32# (word64ToWord# cur)) indent)


{-# INLINE isIndented #-}
isIndented :: Indent -> Cursor -> Bool
isIndented indent cur =
  isTrue# (gtWord32# (wordToWord32# (word64ToWord# cur)) indent)


distanceToIndent :: Indent -> Cursor -> Word32
distanceToIndent indent cur =
  W32# (subWord32# indent (wordToWord32# (word64ToWord# cur)))



-- FUNCTOR


instance Functor (Parser x) where
  {-# INLINE fmap #-}
  fmap f (Parser parser) =
    Parser $ \fpc state cok eok cerr eerr ->
      let
        cok' a s = cok (f a) s
        eok' a s = eok (f a) s
      in
      parser fpc state cok' eok' cerr eerr



-- APPLICATIVE


instance Applicative.Applicative (Parser x) where
  {-# INLINE pure #-}
  pure value =
    Parser $ \_ state _ eok _ _ ->
      eok value state

  {-# INLINE (<*>) #-}
  (<*>) (Parser parserFunc) (Parser parserArg) =
    Parser $ \fpc state cok eok cerr eerr ->
      let
        cokF func s1 =
          let
            cokA arg s2 = cok (func arg) s2
          in
          parserArg fpc s1 cokA cokA cerr cerr

        eokF func s1 =
          let
            cokA arg s2 = cok (func arg) s2
            eokA arg s2 = eok (func arg) s2
          in
          parserArg fpc s1 cokA eokA cerr eerr
      in
      parserFunc fpc state cokF eokF cerr eerr



-- ONE OF


{-# INLINE oneOf #-}
oneOf :: (Cursor -> x) -> [Parser x a] -> Parser x a
oneOf toError parsers =
  Parser $ \fpc state cok eok cerr eerr ->
    oneOfHelp fpc state cok eok cerr eerr toError parsers


oneOfHelp
  :: ForeignPtrContents
  -> State
  -> (a -> State -> IO b)
  -> (a -> State -> IO b)
  -> (Cursor -> (Cursor -> x) -> IO b)
  -> (Cursor -> (Cursor -> x) -> IO b)
  -> (Cursor -> x)
  -> [Parser x a]
  -> IO b
oneOfHelp fpc state cok eok cerr eerr toError parsers =
  case parsers of
    Parser parser : parsers' ->
      parser fpc state cok eok cerr
        (\_ _ -> oneOfHelp fpc state cok eok cerr eerr toError parsers')

    [] ->
      let
        !(State _ _ _ cur) = state
      in
      eerr cur toError



-- ONE OF WITH FALLBACK


{-# INLINE oneOfWithFallback #-}
oneOfWithFallback :: [Parser x a] -> a -> Parser x a -- PERF is this function okay? Worried about allocation/laziness with fallback values.
oneOfWithFallback parsers fallback =
  Parser $ \fpc state cok eok cerr _ ->
    oowfHelp fpc state cok eok cerr parsers fallback


oowfHelp
  :: ForeignPtrContents
  -> State
  -> (a -> State -> IO b)
  -> (a -> State -> IO b)
  -> (Cursor -> (Cursor -> x) -> IO b)
  -> [Parser x a]
  -> a
  -> IO b
oowfHelp fpc state cok eok cerr parsers fallback =
  case parsers of
    [] ->
      eok fallback state

    Parser parser : parsers' ->
      parser fpc state cok eok cerr
        (\_ _ -> oowfHelp fpc state cok eok cerr parsers' fallback)



-- MONAD


instance Monad (Parser x) where
  {-# INLINE (>>=) #-}
  (Parser parserA) >>= callback =
    Parser $ \fpc state cok eok cerr eerr ->
      let
        cok' a s =
          case callback a of
            Parser parserB -> parserB fpc s cok cok cerr cerr

        eok' a s =
          case callback a of
            Parser parserB -> parserB fpc s cok eok cerr eerr
      in
      parserA fpc state cok' eok' cerr eerr



-- FROM BYTESTRING


fromByteString :: Parser x a -> (Cursor -> x) -> B.ByteString -> IO (Either x a)
fromByteString (Parser parser) toBadEnd (B.BS (ForeignPtr pos fpc) (I# len)) =
  do  !result <- parser fpc state toOk' toOk' toErr toErr
      Bytes.touch fpc result
  where
    state = State pos (plusAddr# pos len) 0#Word32 0#Word64
    toOk' = toOk toBadEnd


toOk :: (Cursor -> x) -> a -> State -> IO (Either x a)
toOk toBadEnd !a (State pos end _ cur) =
  if eqAddr pos end
  then return (Right a)
  else return (Left (toBadEnd cur))


toErr :: Cursor -> (Cursor -> x) -> IO (Either x a)
toErr cur toError =
  return (Left (toError cur))



-- FROM SNIPPET


data Snippet =
  Snippet
    { _s_fpc :: ForeignPtrContents
    , _s_pos :: Addr#
    , _s_end :: Addr#
    , _s_cur :: Cursor
    }


fromSnippet :: Parser x a -> (Cursor -> x) -> Snippet -> IO (Either x a)
fromSnippet (Parser parser) toBadEnd (Snippet fpc pos end cur) =
  do  !result <- parser fpc state toOk' toOk' toErr toErr
      Bytes.touch fpc result
  where
    state = State pos end 0#Word32 cur
    toOk' = toOk toBadEnd



-- POSITION


{-# INLINE getPosition #-}
getPosition :: Parser x A.Position
getPosition =
  Parser $ \_ state@(State _ _ _ cur) _ eok _ _ ->
    eok (A.Position cur) state


getRegion :: Parser x () -> Parser x A.Region
getRegion (Parser parser) =
  Parser $ \fpc state@(State _ _ _ start) cok eok cerr eerr ->
    let
      cok' () s@(State _ _ _ end) = cok (A.Region start end) s
      eok' () s@(State _ _ _ end) = eok (A.Region start end) s
    in
    parser fpc state cok' eok' cerr eerr


addLocation :: Parser x a -> Parser x (A.Located a)
addLocation (Parser parser) =
  Parser $ \fpc state@(State _ _ _ start) cok eok cerr eerr ->
    let
      cok' a s@(State _ _ _ end) = cok (A.At (A.Region start end) a) s
      eok' a s@(State _ _ _ end) = eok (A.At (A.Region start end) a) s
    in
    parser fpc state cok' eok' cerr eerr


addEnd :: A.Position -> a -> Parser x (A.Located a)
addEnd start value =
  Parser $ \_ state@(State _ _ _ cur) _ eok _ _ ->
    eok (A.at start (A.Position cur) value) state



-- INDENT


withIndent :: Parser x a -> Parser x a
withIndent (Parser parser) =
  Parser $ \fpc (State pos end oldIndent cur) cok eok cerr eerr ->
    let
      cok' a (State p e _ c) = cok a (State p e oldIndent c)
      eok' a (State p e _ c) = eok a (State p e oldIndent c)
    in
    parser fpc (State pos end (toIndent cur) cur) cok' eok' cerr eerr


withBacksetIndent :: Word32 -> Parser x a -> Parser x a
withBacksetIndent (W32# backset) (Parser parser) =
  Parser $ \fpc (State pos end oldIndent cur) cok eok cerr eerr ->
    let
      cok' a (State p e _ c) = cok a (State p e oldIndent c)
      eok' a (State p e _ c) = eok a (State p e oldIndent c)
    in
    parser fpc (State pos end (subWord32# (toIndent cur) backset) cur) cok' eok' cerr eerr


toIndent :: Cursor -> Indent
toIndent cur =
  wordToWord32# (word64ToWord# cur)



-- CONTEXT


inContext :: (x -> Cursor -> y) -> Parser y start -> Parser x a -> Parser y a
inContext addContext (Parser parserStart) (Parser parserA) =
  Parser $ \fpc state@(State _ _ _ cur) cok eok cerr eerr ->
    let
      cokS _ s = parserA fpc s cok cok (\c tx -> cerr cur (addContext (tx c))) (\c tx -> cerr cur (addContext (tx c)))
      eokS _ s = parserA fpc s cok eok (\c tx -> cerr cur (addContext (tx c))) (\c tx -> eerr cur (addContext (tx c)))
    in
    parserStart fpc state cokS eokS cerr eerr


specialize :: (x -> Cursor -> y) -> Parser x a -> Parser y a
specialize addContext (Parser parser) =
  Parser $ \fpc state@(State _ _ _ cur) cok eok cerr eerr ->
    parser fpc state cok eok
      (\c tx -> cerr cur (addContext (tx c)))
      (\c tx -> eerr cur (addContext (tx c)))



-- SYMBOLS


word1 :: Word8# -> (Cursor -> x) -> Parser x ()
word1 word toError =
  Parser $ \_ (State pos end indent cur) cok _ _ eerr ->
    if ltAddr pos end && eqIndex pos 0# word then
      let !newState = State (plusAddr# pos 1#) end indent (slide cur 1#Word64) in
      cok () newState
    else
      eerr cur toError


word2 :: Word8# -> Word8# -> (Cursor -> x) -> Parser x ()
word2 w1 w2 toError =
  Parser $ \_ (State pos end indent cur) cok _ _ eerr ->
    if ltAddrOff pos 1# end && eqIndex pos 0# w1 && eqIndex pos 1# w2 then
      let !newState = State (plusAddr# pos 2#) end indent (slide cur 2#Word64) in
      cok () newState
    else
      eerr cur toError



-- LOW-LEVEL CHECKS


{-# INLINE eqAddr #-}
{-# INLINE ltAddr #-}
{-# INLINE leAddr #-}
{-# INLINE notLtAddr #-}

eqAddr :: Addr# -> Addr# -> Bool
ltAddr :: Addr# -> Addr# -> Bool
leAddr :: Addr# -> Addr# -> Bool
notLtAddr :: Addr# -> Addr# -> Bool

eqAddr pos end = isTrue# (eqAddr# pos end)
ltAddr pos end = isTrue# (ltAddr# pos end)
leAddr pos end = isTrue# (leAddr# pos end)
notLtAddr pos end = isTrue# (geAddr# pos end)


{-# INLINE ltAddrOff #-}
ltAddrOff :: Addr# -> Int# -> Addr# -> Bool
ltAddrOff pos off end =
  isTrue# (ltAddr# (plusAddr# pos off) end)


{-# INLINE eqIndex #-}
{-# INLINE neIndex #-}

eqIndex :: Addr# -> Int# -> Word8# -> Bool
neIndex :: Addr# -> Int# -> Word8# -> Bool

eqIndex p o w = isTrue# (eqWord8# (indexWord8OffAddr# p o) w)
neIndex p o w = isTrue# (neWord8# (indexWord8OffAddr# p o) w)



-- SKIP UTF8


skipUtf8 :: Addr# -> Addr# -> Word8# -> Addr#
skipUtf8 pos end w
  | isTrue# (eqWord8# w 0x00#Word8) = pos
  | isTrue# (ltWord8# w 0x80#Word8) = plusAddr# pos 1#
  | isTrue# (ltWord8# w 0xE0#Word8) = let !pos2 = plusAddr# pos 2# in if leAddr pos2 end && isTrue# (eqWord8# 0#Word8                                       (stepOff t# pos 1# (step t# w 0#Word8)))   then pos2 else pos
  | isTrue# (ltWord8# w 0xF0#Word8) = let !pos3 = plusAddr# pos 3# in if leAddr pos3 end && isTrue# (eqWord8# 0#Word8                    (stepOff t# pos 2# (stepOff t# pos 1# (step t# w 0#Word8))))  then pos3 else pos
  | otherwise                       = let !pos4 = plusAddr# pos 4# in if leAddr pos4 end && isTrue# (eqWord8# 0#Word8 (stepOff t# pos 3# (stepOff t# pos 2# (stepOff t# pos 1# (step t# w 0#Word8))))) then pos4 else pos
  where
    !(Literals.FSM t#) = Bytes.utf8_fsm


step :: Addr# -> Word8# -> Word8# -> Word8#
step table w state =
  indexWord8OffAddr# table (256# +# word2Int# (word8ToWord# state) +# int8ToInt# (indexInt8OffAddr# table (word2Int# (word8ToWord# w))))


stepOff :: Addr# -> Addr# -> Int# -> Word8# -> Word8#
stepOff table pos off state =
  step table (indexWord8OffAddr# pos off) state

