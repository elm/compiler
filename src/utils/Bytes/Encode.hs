{-# LANGUAGE BangPatterns, CPP, ExtendedLiterals, MagicHash, TemplateHaskell,
UnboxedTuples
#-}
module Bytes.Encode
  ( B.Builder
  , u8, u16, u32, u64
  , i8, i16, i32, i64, iX, int
  , bool, char, chars64
  , maybe
  , list8, list16, list32
  , dict8, dict16, dict32
  , set8, set16, set32
  , array8, array16, array32
  , string8, string16, string32
  , words16LE, words32LE
  , bytes
  , byteString64
  , u8#, u16#, u32#, u64#
  --
  , Fixed(..)
  , u8_
  , withPinned_
  , withPinned
  )
  where


import Prelude hiding (maybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Internal as B
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Bits (shiftR)
import GHC.Exts (Int(..), isTrue#)
import GHC.ForeignPtr (ForeignPtr(ForeignPtr))
import GHC.Int (Int8(..), Int16(..), Int32(..), Int64(..))
import GHC.IO (IO(IO))
import GHC.Prim
import GHC.Ptr (Ptr(Ptr))
import GHC.Word (Word8(..), Word16(..), Word32(..), Word64(..))

import qualified Array as A
import qualified Buffer
import qualified Bytes
import qualified Crash
import qualified String as S

#include <ghcautoconf.h>
-- to import WORDS_BIGENDIAN



-- UNSIGNED


{-# INLINE u8  #-}
{-# INLINE u16 #-}
{-# INLINE u32 #-}
{-# INLINE u64 #-}

u8  :: Word8  -> B.Builder
u16 :: Word16 -> B.Builder
u32 :: Word32 -> B.Builder
u64 :: Word64 -> B.Builder

u8  (W8#  w) = u8#  w
u16 (W16# w) = u16# w
u32 (W32# w) = u32# w
u64 (W64# w) = u64# w



-- SIGNED


{-# INLINE i8  #-}
{-# INLINE i16 #-}
{-# INLINE i32 #-}
{-# INLINE i64 #-}

i8  :: Int8  -> B.Builder
i16 :: Int16 -> B.Builder
i32 :: Int32 -> B.Builder
i64 :: Int64 -> B.Builder

i8  = B.int8
i16 = B.int16LE
i32 = B.int32LE
i64 = B.int64LE



-- INTEGER


iX :: Integer -> B.Builder
iX n =
  if lo <= n && n <= hi
  then
    B.word8 0 <> B.int32LE (fromIntegral n)
  else
    if n > 0
    then B.word8 1 <> go 0   n  mempty
    else B.word8 2 <> go 0 (-n) mempty
  where
    lo = fromIntegral (minBound :: Int32) :: Integer
    hi = fromIntegral (maxBound :: Int32) :: Integer

    go len m builder =
      if m == 0
      then B.word32LE len <> builder
      else go (len + 1) (shiftR m 8) (B.word8 (fromIntegral m) <> builder)



-- INT


{-# INLINE int #-}
int :: Int -> B.Builder
int n =
  i64 (fromIntegral n)



-- BOOL


bool :: Bool -> B.Builder
bool b =
  u8# (if b then 1#Word8 else 0#Word8)



-- CHARS


char :: Char -> B.Builder
char =
  B.charUtf8


chars64 :: [Char] -> B.Builder
chars64 s =
  int (length s) <> B.stringUtf8 s



-- MAYBE


maybe :: (a -> B.Builder) -> Maybe a -> B.Builder
maybe e m =
  case m of
    Nothing -> u8# 0#Word8
    Just a  -> u8# 1#Word8 <> e a



-- LIST


{-# INLINE list8  #-}
{-# INLINE list16 #-}
{-# INLINE list32 #-}

list8  :: (a -> B.Builder) -> [a] -> B.Builder
list16 :: (a -> B.Builder) -> [a] -> B.Builder
list32 :: (a -> B.Builder) -> [a] -> B.Builder

list8  e xs = u8  (fromIntegral (List.length xs)) <> mconcat (List.map e xs)
list16 e xs = u16 (fromIntegral (List.length xs)) <> mconcat (List.map e xs)
list32 e xs = u32 (fromIntegral (List.length xs)) <> mconcat (List.map e xs)



-- DICT


{-# INLINE dict8  #-}
{-# INLINE dict16 #-}
{-# INLINE dict32 #-}

dict8  :: (k -> B.Builder) -> (v -> B.Builder) -> Map.Map k v -> B.Builder
dict16 :: (k -> B.Builder) -> (v -> B.Builder) -> Map.Map k v -> B.Builder
dict32 :: (k -> B.Builder) -> (v -> B.Builder) -> Map.Map k v -> B.Builder

dict8  eK eV d = u8  (fromIntegral (Map.size d)) <> Map.foldrWithKey (\k v b -> eK k <> eV v <> b) mempty d
dict16 eK eV d = u16 (fromIntegral (Map.size d)) <> Map.foldrWithKey (\k v b -> eK k <> eV v <> b) mempty d
dict32 eK eV d = u32 (fromIntegral (Map.size d)) <> Map.foldrWithKey (\k v b -> eK k <> eV v <> b) mempty d



-- SET


{-# INLINE set8  #-}
{-# INLINE set16 #-}
{-# INLINE set32 #-}

set8  :: (a -> B.Builder) -> Set.Set a -> B.Builder
set16 :: (a -> B.Builder) -> Set.Set a -> B.Builder
set32 :: (a -> B.Builder) -> Set.Set a -> B.Builder

set8  e s = u8  (fromIntegral (Set.size s)) <> Set.foldr (\x b -> e x <> b) mempty s
set16 e s = u16 (fromIntegral (Set.size s)) <> Set.foldr (\x b -> e x <> b) mempty s
set32 e s = u32 (fromIntegral (Set.size s)) <> Set.foldr (\x b -> e x <> b) mempty s



-- ARRAY


{-# INLINE array8  #-}
{-# INLINE array16 #-}
{-# INLINE array32 #-}

array8  :: (a -> B.Builder) -> A.Array a -> B.Builder
array16 :: (a -> B.Builder) -> A.Array a -> B.Builder
array32 :: (a -> B.Builder) -> A.Array a -> B.Builder

array8  e a = u8  (fromIntegral (A.size a)) <> A.foldr (\x b -> e x <> b) mempty a
array16 e a = u16 (fromIntegral (A.size a)) <> A.foldr (\x b -> e x <> b) mempty a
array32 e a = u32 (fromIntegral (A.size a)) <> A.foldr (\x b -> e x <> b) mempty a



-- STRING


{-# INLINE string8  #-}
{-# INLINE string16 #-}
{-# INLINE string32 #-}

string8  :: S.String -> B.Builder
string16 :: S.String -> B.Builder
string32 :: S.String -> B.Builder

string8  s = u8  (fromIntegral (S.size s)) <> S.toBuilder s
string16 s = u16 (fromIntegral (S.size s)) <> S.toBuilder s
string32 s = u32 (fromIntegral (S.size s)) <> S.toBuilder s



-- BYTES


{-# INLINE bytes #-}
bytes :: ByteArray# -> B.Builder
bytes =
  \ba# -> B.builder (bytesHelp ba#)


{-# INLINE bytesHelp #-}
bytesHelp :: ByteArray# -> B.BuildStep a -> B.BuildStep a
bytesHelp ba k =
    go 0# (sizeofByteArray# ba)
  where
    go i len (B.BufferRange (Ptr pos) (Ptr end)) =
      let
        space = minusAddr# end pos
      in
      if isTrue# (len <=# space) then
        do  copyToAddr ba i pos len
            let !br = B.BufferRange (Ptr (plusAddr# pos len)) (Ptr end)
            k br
      else
        do  copyToAddr ba i pos space
            return $ B.bufferFull 1 (Ptr end) (go (i +# space) (len -# space))


{-# INLINE copyToAddr #-}
copyToAddr :: ByteArray# -> Int# -> Addr# -> Int# -> IO ()
copyToAddr ba i addr len =
  IO $ \s ->
    case copyByteArrayToAddr# ba i addr len s of
      s1 -> (# s1, () #)



-- BYTESTRING


byteString64 :: BS.ByteString -> B.Builder
byteString64 bs =
  u64 (fromIntegral (BS.length bs)) <> B.byteString bs



-- WORDS


{-# INLINE words16LE #-}
words16LE :: ByteArray# -> B.Builder
words16LE ba =
#if defined(WORDS_BIGENDIAN)
  bytes (Bytes.byteSwaps16# ba)
#else
  bytes ba
#endif


{-# INLINE words32LE #-}
words32LE :: ByteArray# -> B.Builder
words32LE ba =
#if defined(WORDS_BIGENDIAN)
  bytes (Bytes.byteSwaps32# ba)
#else
  bytes ba
#endif



-- UNBOXED UNSIGNED


{-# INLINE u8# #-}
u8# :: Word8#  -> B.Builder
u8# w =
    B.builder go
  where
    go step =
      \(B.BufferRange (Ptr pos) (Ptr end)) ->
        if isTrue# (ltAddr# pos end)
        then
          do  IO (\s0 -> case writeWord8OffAddr# pos 0# w s0 of s1 -> (# s1, () #))
              let !br = B.BufferRange (Ptr (plusAddr# pos 1#)) (Ptr end)
              step br
        else
          return (B.bufferFull 1 (Ptr pos) (go step))


{-# INLINE u16# #-}
u16# :: Word16# -> B.Builder
u16# w =
    B.builder go
  where
    go step =
      \(B.BufferRange (Ptr pos) (Ptr end)) ->
        let !pos2 = plusAddr# pos 2# in
        if isTrue# (leAddr# pos2 end)
        then
          do  Bytes.setWord16_LE pos w
              let !br = B.BufferRange (Ptr pos2) (Ptr end)
              step br
        else
          return (B.bufferFull 2 (Ptr pos) (go step))


{-# INLINE u32# #-}
u32# :: Word32# -> B.Builder
u32# w =
    B.builder go
  where
    go step =
      \(B.BufferRange (Ptr pos) (Ptr end)) ->
        let !pos4 = plusAddr# pos 4# in
        if isTrue# (leAddr# pos4 end)
        then
          do  Bytes.setWord32_LE pos w
              let !br = B.BufferRange (Ptr pos4) (Ptr end)
              step br
        else
          return (B.bufferFull 4 (Ptr pos) (go step))


{-# INLINE u64# #-}
u64# :: Word64# -> B.Builder
u64# w =
    B.builder go
  where
    go step =
      \(B.BufferRange (Ptr pos) (Ptr end)) ->
        let !pos8 = plusAddr# pos 8# in
        if isTrue# (leAddr# pos8 end)
        then
          do  Bytes.setWord64_LE pos w
              let !br = B.BufferRange (Ptr pos8) (Ptr end)
              step br
        else
          return (B.bufferFull 8 (Ptr pos) (go step))



-- FIXED SIZE


data Fixed =
  Fixed Int# B.Builder


instance Semigroup Fixed where
  {-# INLINE (<>) #-}
  (<>) (Fixed n1 b1) (Fixed n2 b2) = Fixed (n1 +# n2) (b1 <> b2)

instance Monoid Fixed where
  {-# INLINE mempty #-}
  mempty = Fixed 0# mempty


{-# INLINE u8_ #-}
u8_ :: Word8 -> Fixed
u8_ w =
  Fixed 1# (u8 w)


withPinned_ :: Fixed -> (Addr# -> Int# -> IO r) -> IO r
withPinned_ fixed cont =
  withPinned fixed (\_ addr len -> cont addr len)


withPinned :: Fixed -> (Buffer.Ref -> Addr# -> Int# -> IO r) -> IO r
withPinned (Fixed size builder) cont =
  Buffer.with# size $ \ref start end _ ->
    do  loop (B.runBuilder builder) (B.BufferRange (Ptr start) (Ptr end))
        a <- cont ref start size
        Buffer.touch ref
        return a
  where
    loop step range@(B.BufferRange _ (Ptr end)) =
      B.fillWithBuildStep step
        (\(Ptr pos) () ->
            if isTrue# (eqAddr# pos end)
            then return ()
            else $(Crash.crashIO 'withPinned) "unexpected Fixed encoder"
        )
        (\_ _ _ ->
            $(Crash.crashIO 'withPinned) "unexpected Fixed encoder"
        )
        (\(Ptr pos) (BS.BS (ForeignPtr addr fpc) (I# len)) nextStep ->
            do  IO $ \s0 ->
                  case copyAddrToAddrNonOverlapping# addr pos len s0 of { s1 ->
                  case touch# fpc                                 s1 of { s2 -> (# s2, () #) }}
                loop nextStep (B.BufferRange (Ptr (plusAddr# pos len)) (Ptr end))
        )
        range

