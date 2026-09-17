{-# LANGUAGE BangPatterns, CPP, MagicHash, Rank2Types, UnboxedTuples #-}
module Bytes.Decode
  ( Decoder
  , expecting
  --
  , u8, u16, u32, u64
  , i8, i16, i32, i64, iX, int
  , u8#, u16#, u32#, u64#
  , i8#, i16#, i32#, i64#
  , bool, char, chars64
  , maybe
  , list8, list16, list32
  , dict8, dict16, dict32
  , set8, set16, set32
  , array8, array16, array32
  , string8, string16, string32
  , words16LE, words32LE
  , bytes, bytes#
  , byteString64
  --
  , map1, map2, map3, map4, map5, map6, map7
  , io
  --
  , fromByteString
  , fromFile
  )
  where


import Prelude hiding (maybe)
import Control.Monad (replicateM)
import Data.Bits ((.|.), shiftL, xor)
import qualified Data.ByteString.Internal as BS
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Exts (Int(..), isTrue#)
import GHC.ForeignPtr (ForeignPtr(ForeignPtr), ForeignPtrContents(PlainPtr))
import GHC.Int (Int8(..), Int16(..), Int32(..), Int64(..))
import GHC.IO (IO(IO))
import GHC.Prim
import GHC.Ptr (Ptr(Ptr))
import GHC.Word (Word8(..), Word16(..), Word32(..), Word64(..))
import qualified System.IO as IO

import qualified Array as A
import qualified Buffer
import qualified Bytes
import qualified String as S

#include <ghcautoconf.h>
-- to import WORDS_BIGENDIAN



-- UINT


{-# INLINE u8  #-}
{-# INLINE u16 #-}
{-# INLINE u32 #-}
{-# INLINE u64 #-}

u8  :: Decoder Word8
u16 :: Decoder Word16
u32 :: Decoder Word32
u64 :: Decoder Word64

u8  = u8# W8#
u16 = u16# W16#
u32 = u32# W32#
u64 = u64# W64#


{-# INLINE u8#  #-}
{-# INLINE u16# #-}
{-# INLINE u32# #-}
{-# INLINE u64# #-}

u8#  :: (Word8#  -> a) -> Decoder a
u16# :: (Word16# -> a) -> Decoder a
u32# :: (Word32# -> a) -> Decoder a
u64# :: (Word64# -> a) -> Decoder a

u8#  f = fixed 1# (Bytes.getWord8 f)
u16# f = fixed 2# (Bytes.getWord16_LE f)
u32# f = fixed 4# (Bytes.getWord32_LE f)
u64# f = fixed 8# (Bytes.getWord64_LE f)



-- INT


{-# INLINE i8  #-}
{-# INLINE i16 #-}
{-# INLINE i32 #-}
{-# INLINE i64 #-}

i8  :: Decoder Int8
i16 :: Decoder Int16
i32 :: Decoder Int32
i64 :: Decoder Int64

i8  = i8# I8#
i16 = i16# I16#
i32 = i32# I32#
i64 = i64# I64#


{-# INLINE i8#  #-}
{-# INLINE i16# #-}
{-# INLINE i32# #-}
{-# INLINE i64# #-}

i8#  :: (Int8# -> a) -> Decoder a
i16# :: (Int16# -> a) -> Decoder a
i32# :: (Int32# -> a) -> Decoder a
i64# :: (Int64# -> a) -> Decoder a

i8#  f = fixed 1# (Bytes.getInt8 f)
i16# f = fixed 2# (Bytes.getInt16_LE f)
i32# f = fixed 4# (Bytes.getInt32_LE f)
i64# f = fixed 8# (Bytes.getInt64_LE f)



-- FIXED SIZE


{-# INLINE fixed #-}
fixed :: Int# -> (Addr# -> IO a) -> Decoder a
fixed size func =
  Decoder $ \pos end ok ->
    let
      !newPos = plusAddr# pos size
    in
    if isTrue# (leAddr# newPos end)
    then ok newPos end =<< func pos
    else return $ Stall pos end (fixedHelp size func ok)


fixedHelp :: Int# -> (Addr# -> IO a) -> Stepper a r -> Continue r
fixedHelp size func ok =
  \pos end ->
    let
      !newPos = plusAddr# pos size
    in
    if isTrue# (leAddr# newPos end)
    then ok newPos end =<< func pos
    else return $ Stall pos end (fixedHelp size func ok)



-- INTEGER


iX :: Decoder Integer
iX =
  do  tag <- u8
      case tag of
        0 -> fromIntegral <$> i32
        1 ->             go 0 =<< u32
        2 -> negate <$> (go 0 =<< u32)
        _ -> expecting "IntX"
  where
    go n len =
      if len == 0
      then return n
      else
        do  w <- u8
            go (shiftL n 8 .|. fromIntegral w) (len - 1)



-- INT


{-# INLINE int #-}
int :: Decoder Int
int =
  i64# (\n -> I# (int64ToInt# n))



-- BOOL


bool :: Decoder Bool
bool =
  do  tag <- u8
      case tag of
        0 -> pure False
        1 -> pure True
        _ -> expecting "Bool"



-- CHARS
--
-- TODO switch to just storing code as u32 later


char :: Decoder Char
char =
  do  w <- u8_int
      case () of
        _ | w < 0x80 ->
              do  return $ Char.chr w

          | w < 0xe0 ->
              do  x <- xor 0x80 <$> u8_int
                  return $ Char.chr $ shiftL (xor 0xc0 w) 6 .|. x

          | w < 0xf0 ->
              do  x <- xor 0x80 <$> u8_int
                  y <- xor 0x80 <$> u8_int
                  return $ Char.chr $ shiftL (xor 0xe0 w) 12 .|. shiftL x 6 .|. y

          | otherwise ->
              do  x <- xor 0x80 <$> u8_int
                  y <- xor 0x80 <$> u8_int
                  z <- xor 0x80 <$> u8_int
                  let code = shiftL (xor 0xf0 w) 18 .|. shiftL x 12 .|. shiftL y 6 .|. z
                  if code <= 0x10FFFF
                    then return $ Char.chr code
                    else expecting "Char"
  where
    u8_int :: Decoder Int
    u8_int =
      fromIntegral <$> u8


chars64 :: Decoder [Char]
chars64 =
  do  len <- u64
      replicateM (fromIntegral len) char



-- MAYBE


maybe :: Decoder a -> Decoder (Maybe a)
maybe d =
  do  tag <- u8
      case tag of
        0 -> pure Nothing
        _ -> Just <$> d



-- LIST


list8  :: Decoder a -> Decoder [a]
list16 :: Decoder a -> Decoder [a]
list32 :: Decoder a -> Decoder [a]

list8  d = list d . fromIntegral =<< u8
list16 d = list d . fromIntegral =<< u16
list32 d = list d . fromIntegral =<< u32


{-# INLINE list #-}
list :: Decoder a -> Word -> Decoder [a]
list decoder n =
    go [] n
  where
    go xs i =
      if i == 0
      then return $! List.reverse xs
      else
        do  x <- decoder
            x `seq` go (x:xs) (i-1)



-- DICT


dict8  :: Decoder k -> Decoder v -> Decoder (Map.Map k v)
dict16 :: Decoder k -> Decoder v -> Decoder (Map.Map k v)
dict32 :: Decoder k -> Decoder v -> Decoder (Map.Map k v)

dict8  k v = dict k v . fromIntegral =<< u8
dict16 k v = dict k v . fromIntegral =<< u16
dict32 k v = dict k v . fromIntegral =<< u32


{-# INLINE dict #-}
dict :: Decoder k -> Decoder v -> Word -> Decoder (Map.Map k v)
dict dK dV n =
    go [] n
  where
    go pairs i =
      if i == 0
      then return $! Map.fromDistinctDescList pairs
      else
        do  k <- dK
            v <- dV
            k `seq` v `seq` go ((k,v):pairs) (i-1)



-- SET


set8  :: Decoder a -> Decoder (Set.Set a)
set16 :: Decoder a -> Decoder (Set.Set a)
set32 :: Decoder a -> Decoder (Set.Set a)

set8  d = set d . fromIntegral =<< u8
set16 d = set d . fromIntegral =<< u16
set32 d = set d . fromIntegral =<< u32


{-# INLINE set #-}
set :: Decoder a -> Word -> Decoder (Set.Set a)
set d n =
    go [] n
  where
    go revs i =
      if i == 0
      then return $! Set.fromDistinctDescList revs
      else
        do  a <- d
            a `seq` go (a:revs) (i-1)



-- ARRAY
--
-- NOTE: it is unsafe to use array16 and array32 with untrusted data. They
-- pre-allocate the destination array which is faster for trusted data, but
-- untrusted data could have false lengths that cause large allocations.


array8  :: Decoder a -> Decoder (A.Array a)
array16 :: Decoder a -> Decoder (A.Array a)
array32 :: Decoder a -> Decoder (A.Array a)

array8  d = array d . fromIntegral =<< u8
array16 d = array d . fromIntegral =<< u16
array32 d = array d . fromIntegral =<< u32


{-# INLINE array #-}
array :: Decoder a -> Int -> Decoder (A.Array a)
array decoder len =
  if len == 0
  then return A.empty
  else
    do  a <- decoder
        arrayHelp decoder len =<< io (newArray len a)


{-# INLINE arrayHelp #-}
arrayHelp :: Decoder a -> Int -> SMA a -> Decoder (A.Array a)
arrayHelp decoder (I# len) (SMA sma) =
    go 1#
  where
    go i =
      if isTrue# (i <# len)
      then
        do  a <- decoder
            io (writeArray sma i a)
            go (i +# 1#)
      else
        io (freezeArray sma)


data SMA a =
  SMA (SmallMutableArray# RealWorld a)


{-# INLINE io #-}
io :: IO a -> Decoder a
io work =
  Decoder $ \p e ok ->
    ok p e =<< work


{-# INLINE newArray #-}
{-# INLINE writeArray #-}
{-# INLINE freezeArray #-}

newArray    :: Int -> a -> IO (SMA a)
writeArray  :: SmallMutableArray# RealWorld a -> Int# -> a -> IO ()
freezeArray :: SmallMutableArray# RealWorld a -> IO (A.Array a)

newArray (I# len) a = IO $ \s0 -> case newSmallArray#   len   a    s0 of { (# s1, sma #) -> (# s1, SMA sma    #) }
writeArray sma i  a = IO $ \s0 -> case writeSmallArray# sma i a    s0 of {    s1         -> (# s1, ()         #) }
freezeArray sma     = IO $ \s0 -> case unsafeFreezeSmallArray# sma s0 of { (# s1, sa  #) -> (# s1, A.Array sa #) }



-- STRING
--
-- NOTE: it is unsafe to use string16 and string32 with untrusted data. They
-- pre-allocate the destination buffer which is faster for trusted data, but
-- untrusted data could have false lengths that cause large allocations.


string8  :: Decoder S.String
string16 :: Decoder S.String
string32 :: Decoder S.String

string8  = bytes S.String . fromIntegral =<< u8
string16 = bytes S.String . fromIntegral =<< u16
string32 = bytes S.String . fromIntegral =<< u32



-- WORDS


{-# INLINE words16LE #-}
words16LE :: (ByteArray# -> a) -> Int# -> Decoder a
words16LE func len =
#if defined(WORDS_BIGENDIAN)
  bytes (\ba -> func (Bytes.byteSwaps16# ba)) (2# *# len)
#else
  bytes# func (2# *# len)
#endif


{-# INLINE words32LE #-}
words32LE :: (ByteArray# -> a) -> Int# -> Decoder a
words32LE func len =
#if defined(WORDS_BIGENDIAN)
  bytes (\ba -> func (Bytes.byteSwaps32# ba)) (4# *# len)
#else
  bytes# func (4# *# len)
#endif



-- BYTES
--
-- NOTE: it is unsafe to use bytes with untrusted data. It pre-allocates the
-- destination buffer which is faster for trusted data, but untrusted data
-- could have false lengths that cause large allocations.


{-# INLINE bytes #-}
bytes :: (ByteArray# -> a) -> Int -> Decoder a
bytes func (I# len) =
  bytes# func len


{-# INLINE bytes# #-}
bytes# :: (ByteArray# -> a) -> Int# -> Decoder a
bytes# func len =
  Decoder $ \pos end ok ->
    let
      !newPos = plusAddr# pos len
    in
    if isTrue# (leAddr# newPos end)
    then
      ok newPos end =<< IO (\s0 ->
        case newByteArray# len                   s0 of { (# s1, mba #) ->
        case copyAddrToByteArray# pos mba 0# len s1 of {    s2         ->
        case unsafeFreezeByteArray# mba          s2 of { (# s3, ba  #) -> (# s3, func ba #) }}}
      )
    else
      do  let !got  = minusAddr# end pos
          let !want = len -# got
          (MBA mba) <- newBytes len
          copyBytes pos mba 0# got
          return $ Stall end end (bytesHelp func mba got want ok)


bytesHelp :: (ByteArray# -> a) -> MutableByteArray# RealWorld -> Int# -> Int# -> Stepper a r -> Addr# -> Addr# -> IO (Result r)
bytesHelp func mba offset want ok pos end =
  let
    !newPos = plusAddr# pos want
  in
  if isTrue# (leAddr# newPos end)
  then
    do  copyBytes pos mba offset want
        ok newPos end =<< freezeBytes func mba
  else
    do  let !got = minusAddr# end pos
        copyBytes pos mba offset got
        return $ Stall end end (bytesHelp func mba (offset +# got) (want -# got) ok)


data MBA =
  MBA (MutableByteArray# RealWorld)


newBytes :: Int# -> IO MBA
newBytes len =
  IO $ \s0 ->
    case newByteArray# len s0 of
      (# s1, mba #) ->
        (# s1, MBA mba #)


copyBytes :: Addr# -> MutableByteArray# RealWorld -> Int# -> Int# -> IO ()
copyBytes addr mba i len =
  IO $ \s0 ->
    case copyAddrToByteArray# addr mba i len s0 of
      s1 ->
        (# s1, () #)


freezeBytes :: (ByteArray# -> a) -> MutableByteArray# RealWorld -> IO a
freezeBytes func mba =
  IO $ \s0 ->
    case unsafeFreezeByteArray# mba s0 of
      (# s1, ba #) ->
        (# s1, func ba #)



-- BYTESTRING


byteString64 :: Decoder BS.ByteString
byteString64 =
  do  len <- u64
      byteString (fromIntegral len)


byteString :: Int -> Decoder BS.ByteString
byteString (I# len) =
  Decoder $ \pos end ok ->
    let
      !newPos = plusAddr# pos len
    in
    if isTrue# (leAddr# newPos end)
    then
      ok newPos end =<< newPinnedCopy pos len (\mba -> mkBS mba len)
    else
      do  let !got  = minusAddr# end pos
          let !want = len -# got
          (MBA mba) <- newPinnedCopy pos got MBA
          return $ Stall end end (byteStringHelp mba len got want ok)
  where
    newPinnedCopy pos size func =
      IO $ \s0 ->
        case newPinnedByteArray# len              s0 of { (# s1, mba #) ->
        case copyAddrToByteArray# pos mba 0# size s1 of {    s2         -> (# s2, func mba #) }}




byteStringHelp :: MutableByteArray# RealWorld -> Int# -> Int# -> Int# -> Stepper BS.ByteString r -> Addr# -> Addr# -> IO (Result r)
byteStringHelp mba len offset want ok pos end =
  let
    !newPos = plusAddr# pos want
  in
  if isTrue# (leAddr# newPos end)
  then
    do  copyBytes pos mba offset want
        ok newPos end $ mkBS mba len
  else
    do  let !got = minusAddr# end pos
        copyBytes pos mba offset got
        return $ Stall end end (byteStringHelp mba len (offset +# got) (want -# got) ok)


mkBS :: MutableByteArray# RealWorld -> Int# -> BS.ByteString
mkBS mba len =
  BS.BS (ForeignPtr (mutableByteArrayContents# mba) (PlainPtr mba)) (I# len)



-- DECODER
--
-- Decoders work from a fixed size buffer. This way files can be read in chunks
-- without any new allocations. It also allows leftover bytes to be copied into
-- the next buffer frame very easily.
--
-- The Stall address determines what gets copied into the next buffer frame.
-- This is used by fixed size decoders (8 bytes or less) and should not be used
-- otherwise because:
--
--   1. memcpy is not safe for overlapping memory regions.
--   2. With larger values it makes sense to skip the buffer copy.
--      Just copy the bytes to their final destination directly.
--


newtype Decoder a =
  Decoder ( forall r. Addr# -> Addr# -> Stepper a r -> IO (Result r) )


type Stepper a r =
  Addr# -> Addr# -> a -> IO (Result r)


type Continue a =
  Addr# -> Addr# -> IO (Result a)


data Result a
  = Ok a
  | Stall Addr# Addr# (Continue a)
  | Expecting String


expecting :: String -> Decoder a
expecting msg =
  Decoder $ \_ _ _ ->
    return (Expecting msg)



-- INSTANCES


instance Functor Decoder where
  {-# INLINE fmap #-}
  fmap func (Decoder k) =
    Decoder $ \pos end ok ->
      let
        ok' p e value = ok p e (func value)
      in
      k pos end ok'


instance Applicative Decoder where
  {-# INLINE pure #-}
  pure a =
    Decoder $ \pos end ok ->
      ok pos end a

  {-# INLINE (<*>) #-}
  (<*>) (Decoder kF) (Decoder kV) =
    Decoder $ \pos end ok ->
      let
        okF p e func =
          let
            okV pV eV value = ok pV eV (func value)
          in
          kV p e okV
      in
      kF pos end okF


instance Monad Decoder where
  {-# INLINE (>>=) #-}
  (>>=) (Decoder kA) callback =
    Decoder $ \pos end ok ->
      let
        okA p e a =
          case callback a of
            Decoder kB -> kB p e ok
      in
      kA pos end okA



{-# INLINE map1 #-}
map1 :: (a -> b) -> Decoder a -> Decoder b
map1 func (Decoder k) =
  Decoder $ \pos end ok ->
    let
      ok' p e value = ok p e (func value)
    in
    k pos end ok'


{-# INLINE map2 #-}
map2 :: (a -> b -> v) -> Decoder a -> Decoder b -> Decoder v
map2 func dA dB =
  do  a <- dA
      b <- dB
      return (func a b)


{-# INLINE map3 #-}
map3 :: (a -> b -> c -> v) -> Decoder a -> Decoder b -> Decoder c -> Decoder v
map3 func dA dB dC =
  do  a <- dA
      b <- dB
      c <- dC
      return (func a b c)


{-# INLINE map4 #-}
map4 :: (a -> b -> c -> d -> v) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder v
map4 func dA dB dC dD =
  do  a <- dA
      b <- dB
      c <- dC
      d <- dD
      return (func a b c d)


{-# INLINE map5 #-}
map5 :: (a -> b -> c -> d -> e -> v) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder v
map5 func dA dB dC dD dE =
  do  a <- dA
      b <- dB
      c <- dC
      d <- dD
      e <- dE
      return (func a b c d e)


{-# INLINE map6 #-}
map6 :: (a -> b -> c -> d -> e -> f -> v) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder v
map6 func dA dB dC dD dE dF =
  do  a <- dA
      b <- dB
      c <- dC
      d <- dD
      e <- dE
      f <- dF
      return (func a b c d e f)


{-# INLINE map7 #-}
map7 :: (a -> b -> c -> d -> e -> f -> g -> v) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder v
map7 func dA dB dC dD dE dF dG =
  do  a <- dA
      b <- dB
      c <- dC
      d <- dD
      e <- dE
      f <- dF
      g <- dG
      return (func a b c d e f g)



-- FROM BYTESTRING


fromByteString :: Decoder a -> BS.ByteString -> IO (Either String a)
fromByteString (Decoder cont) (BS.BS (ForeignPtr addr fpc) (I# len)) =
  do  result <- cont addr (plusAddr# addr len) finalize
      Bytes.touch fpc ()
      case result of
        Ok !a         -> return $ Right a
        Stall _ _ _   -> return $ Left "fewer bytes"
        Expecting msg -> return $ Left msg


finalize :: Addr# -> Addr# -> a -> IO (Result a)
finalize pos end value =
  if isTrue# (eqAddr# pos end)
  then return $ Ok value
  else return $ Expecting "fewer bytes"



-- FROM FILE


fromFile :: Decoder a -> FilePath -> IO (Either String a)
fromFile decoder path =
  IO.withBinaryFile path IO.ReadMode $ \handle ->
    Buffer.with32kb $ \ref start _ len ->
      do  result <- readLoop handle start len decoder
          Buffer.touch ref
          return result


{-# INLINE readLoop #-}
readLoop :: IO.Handle -> Addr# -> Int# -> Decoder a -> IO (Either String a)
readLoop handle start len (Decoder cont) =
  do  (I# got) <- IO.hGetBuf handle (Ptr start) (I# len)
      loop got =<< cont start (plusAddr# start got) finalize
  where
    loop have result =
      case result of
        Ok !a ->
          if isTrue# (have <# len)
          then return $ Right a
          else
            do  (I# leftover) <- IO.hGetBuf handle (Ptr start) 1
                case leftover of
                  0# -> return $ Right a
                  _  -> return $ Left "fewer bytes"

        Stall pos end k ->
          if isTrue# (ltAddr# pos end)
          then
            do  let !leftover = minusAddr# end pos
                copy pos start leftover
                (I# new) <- IO.hGetBuf handle (Ptr (plusAddr# start leftover)) (I# (len -# leftover))
                let !got = leftover +# new
                loop got =<< k start (plusAddr# start got)
          else
            do  (I# got) <- IO.hGetBuf handle (Ptr start) (I# len)
                loop got =<< k start (plusAddr# start got)

        Expecting msg ->
          return $ Left msg


copy :: Addr# -> Addr# -> Int# -> IO ()
copy src dst len =
  IO $ \s0 ->
    case copyAddrToAddr# src dst len s0 of
      s1 -> (# s1, () #)

