{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Buffer
  ( Ref
  , touch
  --
  , with
  , with#
  , with1kb
  , with4kb
  , with8kb
  , with16kb
  , with32kb
  , with64kb
  --
  , _1kb
  , _4kb
  , _8kb
  , _16kb
  , _32kb
  , _64kb
  --
  , moreThan
  )
  where


import qualified Data.ByteString.Builder.Internal as B
import GHC.Exts (Int(..))
import GHC.IO (IO(IO))
import GHC.Prim



-- BUFFERS


type Ref =
  MutableByteArray# RealWorld


touch :: Ref -> IO ()
touch ref =
  IO $ \s0 ->
    case touch# ref s0 of
      s1 -> (# s1, () #)



-- WITH


with :: Int -> (Ref -> Addr# -> Addr# -> Int# -> IO a) -> IO a
with (I# len) cont =
  with# len cont


with# :: Int# -> (Ref -> Addr# -> Addr# -> Int# -> IO a) -> IO a
with# len cont =
  IO $ \s0 ->
    case newPinnedByteArray# len s0    of { (# s1, mba #) ->
    case mutableByteArrayContents# mba of {        pos    ->
    case plusAddr# pos len             of {        end    ->
    case cont mba pos end len          of {        IO k   -> k s1 }}}}


with1kb  :: (Ref -> Addr# -> Addr# -> Int# -> IO a) -> IO a
with4kb  :: (Ref -> Addr# -> Addr# -> Int# -> IO a) -> IO a
with8kb  :: (Ref -> Addr# -> Addr# -> Int# -> IO a) -> IO a
with16kb :: (Ref -> Addr# -> Addr# -> Int# -> IO a) -> IO a
with32kb :: (Ref -> Addr# -> Addr# -> Int# -> IO a) -> IO a
with64kb :: (Ref -> Addr# -> Addr# -> Int# -> IO a) -> IO a

with1kb  = with _1kb
with4kb  = with _4kb
with8kb  = with _8kb
with16kb = with _16kb
with32kb = with _32kb
with64kb = with _64kb


_1kb  :: Int
_4kb  :: Int
_8kb  :: Int
_16kb :: Int
_32kb :: Int
_64kb :: Int

_1kb  =      1024 - B.chunkOverhead
_4kb  =  4 * 1024 - B.chunkOverhead
_8kb  =  8 * 1024 - B.chunkOverhead
_16kb = 16 * 1024 - B.chunkOverhead
_32kb = 32 * 1024 - B.chunkOverhead
_64kb = 64 * 1024 - B.chunkOverhead



-- MORE THAN
--
-- Find a size that is larger than the given number of bytes and is a
-- multiple of the GHC block size of 4kb.
--


moreThan :: Int -> Int
moreThan n =
  if n < 4096 - B.chunkOverhead
  then 4096 - B.chunkOverhead
  else ((n + B.chunkOverhead - 1) `div` 4096 + 1) * 4096 - B.chunkOverhead

