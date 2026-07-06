{-# LANGUAGE BangPatterns, CPP, ExtendedLiterals, ForeignFunctionInterface, MagicHash,
QuasiQuotes, UnboxedTuples, UnliftedFFITypes
#-}
module Bytes
  ( toBuilder#
  , equal
  , compareFast
  , compareLexical
  --
  , getWord8
  , getWord16_LE, getWord32_LE, getWord64_LE
  , getWord16_BE, getWord32_BE, getWord64_BE
  --
  , getInt8
  , getInt16_LE, getInt32_LE, getInt64_LE
  , getInt16_BE, getInt32_BE, getInt64_BE
  --
  , setWord8
  , setWord16_LE, setWord32_LE, setWord64_LE
  , setWord16_BE, setWord32_BE, setWord64_BE
  --
  , setInt8
  , setInt16_LE, setInt32_LE, setInt64_LE
  , setInt16_BE, setInt32_BE, setInt64_BE
  --
  , setWord16_LE#, setWord32_LE#, setWord64_LE#
  , setWord16_BE#, setWord32_BE#, setWord64_BE#
  --
  , setInt16_LE#, setInt32_LE#, setInt64_LE#
  , setInt16_BE#, setInt32_BE#, setInt64_BE#
  --
  , getFloat32, getFloat64
  , setFloat32, setFloat64
  --
  , byteSwaps16#
  , byteSwaps32#
  --
  , touch
  --
  , toHex
  , fromHex
  --
  , utf8_fsm
  )
  where


import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Internal as B
import GHC.Exts (isTrue#)
import GHC.Float (Float(..), Double(..))
import GHC.ForeignPtr (ForeignPtrContents)
import GHC.IO (IO(IO))
import GHC.Prim
import GHC.Ptr (Ptr(Ptr))
import GHC.ST (ST(ST), runST)

import Literals (fsm)
import qualified Literals

#include <ghcautoconf.h>
-- to import WORDS_BIGENDIAN



-- TO BUILDER


{-# INLINE toBuilder# #-}
toBuilder# :: ByteArray# -> B.Builder
toBuilder# ba# =
  B.builder (toBuilderHelp 0# (sizeofByteArray# ba#) ba#)


{-# INLINE toBuilderHelp #-}
toBuilderHelp :: Int# -> Int# -> ByteArray# -> B.BuildStep a -> B.BuildStep a
toBuilderHelp start# length# ba# k =
    go start# length#
  where
    go i# len# (B.BufferRange (Ptr pos#) end@(Ptr end#)) =
      let
        space# = minusAddr# end# pos#
      in
      if isTrue# (len# <=# space#) then
        do  copyToAddr ba# i# pos# len#
            let !br = B.BufferRange (Ptr (plusAddr# pos# len#)) end
            k br
      else
        do  copyToAddr ba# i# pos# space#
            return $ B.bufferFull 1 end (go (i# +# space#) (len# -# space#))


{-# INLINE copyToAddr #-}
copyToAddr :: ByteArray# -> Int# -> Addr# -> Int# -> IO ()
copyToAddr ba# i# addr# len# =
  IO $ \s ->
    case copyByteArrayToAddr# ba# i# addr# len# s of
      s1 ->
        (# s1, () #)



-- EQUAL


{-# INLINE equal #-}
equal :: ByteArray# -> ByteArray# -> Bool
equal ba1 ba2 =
  let
    !len1 = sizeofByteArray# ba1
    !len2 = sizeofByteArray# ba2
  in
  isTrue# (len1 ==# len2)
  &&
  isTrue# (0# ==# compareByteArrays# ba1 0# ba2 0# len1)



-- COMPARE FAST


{-# INLINE compareFast #-}
compareFast :: ByteArray# -> ByteArray# -> Ordering
compareFast ba1 ba2 =
  let
    !len1 = sizeofByteArray# ba1
    !len2 = sizeofByteArray# ba2
  in
  if isTrue# (len1 <# len2) then LT else
  if isTrue# (len1 ># len2) then GT else
    let
      !cmp = compareByteArrays# ba1 0# ba2 0# len1
    in
    if isTrue# (cmp <# 0#) then LT else
    if isTrue# (cmp ># 0#) then GT else EQ



-- COMPARE LEXICAL


{-# INLINE compareLexical #-}
compareLexical :: ByteArray# -> ByteArray# -> Ordering
compareLexical ba1 ba2 =
  let
    !len1 = sizeofByteArray# ba1
    !len2 = sizeofByteArray# ba2
    !len  = if isTrue# (len1 <# len2) then len1 else len2
    !cmp  = compareByteArrays# ba1 0# ba2 0# len
  in
  case () of
    _ | isTrue# (cmp <# 0#)    -> LT
      | isTrue# (cmp ># 0#)    -> GT
      | isTrue# (len1 <# len2) -> LT
      | isTrue# (len1 ># len2) -> GT
      | True                   -> EQ



-- GET BYTES


{-# INLINE getWord8     #-}
{-# INLINE getWord16_LE #-}
{-# INLINE getWord32_LE #-}
{-# INLINE getWord64_LE #-}
{-# INLINE getWord16_BE #-}
{-# INLINE getWord32_BE #-}
{-# INLINE getWord64_BE #-}

{-# INLINE getInt8     #-}
{-# INLINE getInt16_LE #-}
{-# INLINE getInt32_LE #-}
{-# INLINE getInt64_LE #-}
{-# INLINE getInt16_BE #-}
{-# INLINE getInt32_BE #-}
{-# INLINE getInt64_BE #-}

getWord8     :: (Word8#  -> a) -> Addr# -> IO a
getWord16_LE :: (Word16# -> a) -> Addr# -> IO a
getWord32_LE :: (Word32# -> a) -> Addr# -> IO a
getWord64_LE :: (Word64# -> a) -> Addr# -> IO a
getWord16_BE :: (Word16# -> a) -> Addr# -> IO a
getWord32_BE :: (Word32# -> a) -> Addr# -> IO a
getWord64_BE :: (Word64# -> a) -> Addr# -> IO a

getInt8      :: (Int8#  -> a) -> Addr# -> IO a
getInt16_LE  :: (Int16# -> a) -> Addr# -> IO a
getInt32_LE  :: (Int32# -> a) -> Addr# -> IO a
getInt64_LE  :: (Int64# -> a) -> Addr# -> IO a
getInt16_BE  :: (Int16# -> a) -> Addr# -> IO a
getInt32_BE  :: (Int32# -> a) -> Addr# -> IO a
getInt64_BE  :: (Int64# -> a) -> Addr# -> IO a

getWord8 f a = IO $ \s0 -> case readWord8OffAddr# a 0# s0 of { (# s1, w #) -> (# s1, f w #) }
getInt8  f a = IO $ \s0 -> case readInt8OffAddr#  a 0# s0 of { (# s1, i #) -> (# s1, f i #) }

#if defined(WORDS_BIGENDIAN)
getWord16_LE f a = IO $ \s0 -> case readWord16OffAddr# a 0# s0 of { (# s1, w #) -> (# s1, f (wordToWord16# (byteSwap16# (word16ToWord# w))) #) }
getWord32_LE f a = IO $ \s0 -> case readWord32OffAddr# a 0# s0 of { (# s1, w #) -> (# s1, f (wordToWord32# (byteSwap32# (word32ToWord# w))) #) }
getWord64_LE f a = IO $ \s0 -> case readWord64OffAddr# a 0# s0 of { (# s1, w #) -> (# s1, f (              (byteSwap64# (              w))) #) }
getWord16_BE f a = IO $ \s0 -> case readWord16OffAddr# a 0# s0 of { (# s1, w #) -> (# s1, f w #) }
getWord32_BE f a = IO $ \s0 -> case readWord32OffAddr# a 0# s0 of { (# s1, w #) -> (# s1, f w #) }
getWord64_BE f a = IO $ \s0 -> case readWord64OffAddr# a 0# s0 of { (# s1, w #) -> (# s1, f w #) }

getInt16_LE f a = IO $ \s0 -> case readWord16OffAddr# a 0# s0 of { (# s1, w #) -> (# s1, f (intToInt16# (word2Int# (byteSwap16# (word16ToWord# w)))) #) }
getInt32_LE f a = IO $ \s0 -> case readWord32OffAddr# a 0# s0 of { (# s1, w #) -> (# s1, f (intToInt32# (word2Int# (byteSwap32# (word32ToWord# w)))) #) }
getInt64_LE f a = IO $ \s0 -> case readWord64OffAddr# a 0# s0 of { (# s1, w #) -> (# s1, f (word64ToInt64#         (byteSwap64# (              w)) ) #) }
getInt16_BE f a = IO $ \s0 -> case readInt16OffAddr#  a 0# s0 of { (# s1, i #) -> (# s1, f i #) }
getInt32_BE f a = IO $ \s0 -> case readInt32OffAddr#  a 0# s0 of { (# s1, i #) -> (# s1, f i #) }
getInt64_BE f a = IO $ \s0 -> case readInt64OffAddr#  a 0# s0 of { (# s1, i #) -> (# s1, f i #) }
#else
getWord16_LE f a = IO $ \s0 -> case readWord16OffAddr# a 0# s0 of { (# s1, w #) -> (# s1, f w #) }
getWord32_LE f a = IO $ \s0 -> case readWord32OffAddr# a 0# s0 of { (# s1, w #) -> (# s1, f w #) }
getWord64_LE f a = IO $ \s0 -> case readWord64OffAddr# a 0# s0 of { (# s1, w #) -> (# s1, f w #) }
getWord16_BE f a = IO $ \s0 -> case readWord16OffAddr# a 0# s0 of { (# s1, w #) -> (# s1, f (wordToWord16# (byteSwap16# (word16ToWord# w))) #) }
getWord32_BE f a = IO $ \s0 -> case readWord32OffAddr# a 0# s0 of { (# s1, w #) -> (# s1, f (wordToWord32# (byteSwap32# (word32ToWord# w))) #) }
getWord64_BE f a = IO $ \s0 -> case readWord64OffAddr# a 0# s0 of { (# s1, w #) -> (# s1, f (              (byteSwap64# (              w))) #) }

getInt16_LE f a = IO $ \s0 -> case readInt16OffAddr#  a 0# s0 of { (# s1, i #) -> (# s1, f i #) }
getInt32_LE f a = IO $ \s0 -> case readInt32OffAddr#  a 0# s0 of { (# s1, i #) -> (# s1, f i #) }
getInt64_LE f a = IO $ \s0 -> case readInt64OffAddr#  a 0# s0 of { (# s1, i #) -> (# s1, f i #) }
getInt16_BE f a = IO $ \s0 -> case readWord16OffAddr# a 0# s0 of { (# s1, w #) -> (# s1, f (intToInt16# (word2Int# (byteSwap16# (word16ToWord# w)))) #) }
getInt32_BE f a = IO $ \s0 -> case readWord32OffAddr# a 0# s0 of { (# s1, w #) -> (# s1, f (intToInt32# (word2Int# (byteSwap32# (word32ToWord# w)))) #) }
getInt64_BE f a = IO $ \s0 -> case readWord64OffAddr# a 0# s0 of { (# s1, w #) -> (# s1, f (word64ToInt64#         (byteSwap64# (              w)) ) #) }
#endif



-- SET BYTES


{-# INLINE setWord8 #-}
{-# INLINE setInt8  #-}

setWord8 :: Addr# -> Word8# -> IO ()
setInt8  :: Addr# -> Int8#  -> IO ()

setWord8 p w = IO $ \s0 -> case writeWord8OffAddr# p 0# w s0 of { s1 -> (# s1, () #) }
setInt8  p w = IO $ \s0 -> case writeInt8OffAddr#  p 0# w s0 of { s1 -> (# s1, () #) }

{-# INLINE setWord16_LE #-}
{-# INLINE setWord32_LE #-}
{-# INLINE setWord64_LE #-}
{-# INLINE setWord16_BE #-}
{-# INLINE setWord32_BE #-}
{-# INLINE setWord64_BE #-}

{-# INLINE setInt16_LE #-}
{-# INLINE setInt32_LE #-}
{-# INLINE setInt64_LE #-}
{-# INLINE setInt16_BE #-}
{-# INLINE setInt32_BE #-}
{-# INLINE setInt64_BE #-}

setWord16_LE :: Addr# -> Word16# -> IO ()
setWord32_LE :: Addr# -> Word32# -> IO ()
setWord64_LE :: Addr# -> Word64# -> IO ()
setWord16_BE :: Addr# -> Word16# -> IO ()
setWord32_BE :: Addr# -> Word32# -> IO ()
setWord64_BE :: Addr# -> Word64# -> IO ()

setInt16_LE :: Addr# -> Int16# -> IO ()
setInt32_LE :: Addr# -> Int32# -> IO ()
setInt64_LE :: Addr# -> Int64# -> IO ()
setInt16_BE :: Addr# -> Int16# -> IO ()
setInt32_BE :: Addr# -> Int32# -> IO ()
setInt64_BE :: Addr# -> Int64# -> IO ()

setWord16_LE p w = IO $ \s0 -> case setWord16_LE# p w s0 of { s1 -> (# s1, () #) }
setWord32_LE p w = IO $ \s0 -> case setWord32_LE# p w s0 of { s1 -> (# s1, () #) }
setWord64_LE p w = IO $ \s0 -> case setWord64_LE# p w s0 of { s1 -> (# s1, () #) }
setWord16_BE p w = IO $ \s0 -> case setWord16_BE# p w s0 of { s1 -> (# s1, () #) }
setWord32_BE p w = IO $ \s0 -> case setWord32_BE# p w s0 of { s1 -> (# s1, () #) }
setWord64_BE p w = IO $ \s0 -> case setWord64_BE# p w s0 of { s1 -> (# s1, () #) }

setInt16_LE p w = IO $ \s0 -> case setInt16_LE# p w s0 of { s1 -> (# s1, () #) }
setInt32_LE p w = IO $ \s0 -> case setInt32_LE# p w s0 of { s1 -> (# s1, () #) }
setInt64_LE p w = IO $ \s0 -> case setInt64_LE# p w s0 of { s1 -> (# s1, () #) }
setInt16_BE p w = IO $ \s0 -> case setInt16_BE# p w s0 of { s1 -> (# s1, () #) }
setInt32_BE p w = IO $ \s0 -> case setInt32_BE# p w s0 of { s1 -> (# s1, () #) }
setInt64_BE p w = IO $ \s0 -> case setInt64_BE# p w s0 of { s1 -> (# s1, () #) }



-- SET BYTES HELPERS


{-# INLINE setWord16_LE# #-}
{-# INLINE setWord32_LE# #-}
{-# INLINE setWord64_LE# #-}
{-# INLINE setWord16_BE# #-}
{-# INLINE setWord32_BE# #-}
{-# INLINE setWord64_BE# #-}

{-# INLINE setInt16_LE# #-}
{-# INLINE setInt32_LE# #-}
{-# INLINE setInt64_LE# #-}
{-# INLINE setInt16_BE# #-}
{-# INLINE setInt32_BE# #-}
{-# INLINE setInt64_BE# #-}

setWord16_LE# :: Addr# -> Word16# -> State# s -> State# s
setWord32_LE# :: Addr# -> Word32# -> State# s -> State# s
setWord64_LE# :: Addr# -> Word64# -> State# s -> State# s
setWord16_BE# :: Addr# -> Word16# -> State# s -> State# s
setWord32_BE# :: Addr# -> Word32# -> State# s -> State# s
setWord64_BE# :: Addr# -> Word64# -> State# s -> State# s

setInt16_LE#  :: Addr# -> Int16#  -> State# s -> State# s
setInt32_LE#  :: Addr# -> Int32#  -> State# s -> State# s
setInt64_LE#  :: Addr# -> Int64#  -> State# s -> State# s
setInt16_BE#  :: Addr# -> Int16#  -> State# s -> State# s
setInt32_BE#  :: Addr# -> Int32#  -> State# s -> State# s
setInt64_BE#  :: Addr# -> Int64#  -> State# s -> State# s

#if defined(WORDS_BIGENDIAN)
setWord16_LE# p w = writeWord16OffAddr# p 0# (wordToWord16# (byteSwap16# (word16ToWord# w)))
setWord32_LE# p w = writeWord32OffAddr# p 0# (wordToWord32# (byteSwap32# (word32ToWord# w)))
setWord64_LE# p w = writeWord64OffAddr# p 0# (byteSwap64# w)
setWord16_BE# p w = writeWord16OffAddr# p 0# w
setWord32_BE# p w = writeWord32OffAddr# p 0# w
setWord64_BE# p w = writeWord64OffAddr# p 0# w

setInt16_LE# p w = setWord16_LE# p (int16ToWord16# w)
setInt32_LE# p w = setWord32_LE# p (int32ToWord32# w)
setInt64_LE# p w = setWord64_LE# p (int64ToWord64# w)
setInt16_BE# p w = writeInt16OffAddr# p 0# w
setInt32_BE# p w = writeInt32OffAddr# p 0# w
setInt64_BE# p w = writeInt64OffAddr# p 0# w
#else
setWord16_LE# p w = writeWord16OffAddr# p 0# w
setWord32_LE# p w = writeWord32OffAddr# p 0# w
setWord64_LE# p w = writeWord64OffAddr# p 0# w
setWord16_BE# p w = writeWord16OffAddr# p 0# (wordToWord16# (byteSwap16# (word16ToWord# w)))
setWord32_BE# p w = writeWord32OffAddr# p 0# (wordToWord32# (byteSwap32# (word32ToWord# w)))
setWord64_BE# p w = writeWord64OffAddr# p 0# (byteSwap64# w)

setInt16_LE# p w = writeInt16OffAddr# p 0# w
setInt32_LE# p w = writeInt32OffAddr# p 0# w
setInt64_LE# p w = writeInt64OffAddr# p 0# w
setInt16_BE# p w = setWord16_BE# p (int16ToWord16# w)
setInt32_BE# p w = setWord32_BE# p (int32ToWord32# w)
setInt64_BE# p w = setWord64_BE# p (int64ToWord64# w)
#endif



-- FLOATING POINT

{-# INLINE getFloat32 #-}
{-# INLINE getFloat64 #-}

getFloat32 :: Addr# -> IO Float
getFloat64 :: Addr# -> IO Double

getFloat32 a = IO $ \s0 -> case readFloatOffAddr#  a 0# s0 of { (# s1, f #) -> (# s1, F# f #) }
getFloat64 a = IO $ \s0 -> case readDoubleOffAddr# a 0# s0 of { (# s1, d #) -> (# s1, D# d #) }


{-# INLINE setFloat32 #-}
{-# INLINE setFloat64 #-}

setFloat32 :: Addr# -> Float#  -> IO ()
setFloat64 :: Addr# -> Double# -> IO ()

setFloat32 a f = IO $ \s0 -> case writeFloatOffAddr#  a 0# f s0 of { s1 -> (# s1, () #) }
setFloat64 a d = IO $ \s0 -> case writeDoubleOffAddr# a 0# d s0 of { s1 -> (# s1, () #) }



-- BYTE SWAPS


data Bytes =
  Bytes ByteArray#


byteSwaps16# :: ByteArray# -> ByteArray#
byteSwaps16# src =
    dst
  where
    !size = sizeofByteArray# src
    !len  = size `quotInt#` 2#

    !(Bytes dst) =
      runST $ ST $ \s0 ->
        case newByteArray# size         s0 of { (# s1, mba #) ->
        case loop mba 0#                s1 of {    s2         ->
        case unsafeFreezeByteArray# mba s2 of { (# s3, ba  #) -> (# s3, Bytes ba #) }}}

    loop mba i s0 =
      if isTrue# (i <# len)
      then
        let
          !before = indexWord16Array# src i
          !after  = wordToWord16# (byteSwap16# (word16ToWord# before))
        in
        case writeWord16Array# mba i after s0 of
          s1 ->
            loop mba (i +# 1#) s1
      else
        setByteArray# mba (2# *# len) size 0# s0


byteSwaps32# :: ByteArray# -> ByteArray#
byteSwaps32# src =
    dst
  where
    !size = sizeofByteArray# src
    !len  = size `quotInt#` 4#

    !(Bytes dst) =
      runST $ ST $ \s0 ->
        case newByteArray# size         s0 of { (# s1, mba #) ->
        case loop mba 0#                s1 of {    s2         ->
        case unsafeFreezeByteArray# mba s2 of { (# s3, ba  #) -> (# s3, Bytes ba #) }}}

    loop mba i s0 =
      if isTrue# (i <# len)
      then
        let
          !before = indexWord32Array# src i
          !after  = wordToWord32# (byteSwap32# (word32ToWord# before))
        in
        case writeWord32Array# mba i after s0 of
          s1 ->
            loop mba (i +# 1#) s1
      else
        setByteArray# mba (4# *# len) size 0# s0



-- TOUCH


{-# INLINE touch #-}
touch :: ForeignPtrContents -> a -> IO a
touch fpc a =
  IO $ \s0 -> case touch# fpc s0 of { s1 -> (# s1, a #) }



-- TO HEX


toHex :: ByteArray# -> (ByteArray# -> a) -> a
toHex ba mk =
  runST $ ST $ \s0 ->
    case newByteArray# size         s0 of { (# s1, mba     #) ->
    case loop mba 0#                s1 of {    s2             ->
    case unsafeFreezeByteArray# mba s2 of { (# s3, hex_ba  #) ->
      (# s3, mk hex_ba #)
    }}}
  where
    !len = sizeofByteArray# ba
    !size = 2# *# len

    loop mba i s0 =
      if isTrue# (i <# len)
      then
        case indexWord8Array# ba i                          of { w  ->
        case writeWord8Array# mba (2# *# i      ) (hi w) s0 of { s1 ->
        case writeWord8Array# mba (2# *# i +# 1#) (lo w) s1 of { s2 ->
          loop mba (i +# 1#) s2
        }}}
      else
        s0

    !letters = "0123456789ABCDEF"#

    hi w = indexWord8OffAddr# letters (word2Int# (word8ToWord# (uncheckedShiftRLWord8# w 4#)))
    lo w = indexWord8OffAddr# letters (word2Int# (word8ToWord# (andWord8# w 0xF#Word8)))


fromHex :: Addr# -> Int# -> (ByteArray# -> a) -> ST s (Maybe a)
fromHex addr len mk =
  if isTrue# (len ># 0#) && isTrue# (remInt# len 2# ==# 0#)
    then
      ST $ \s0 ->
        case newByteArray# (quotInt# len 2#) s0 of
          (# s1, mba #) -> loop mba 0# 0#Word8 s1
    else
      pure Nothing
  where
    !(Literals.Table tbl) = hex_table

    loop mba i state s0 =
      if isTrue# (i <# len)
      then
        let
          !hi = Literals.read tbl (indexWord8OffAddr# addr (i      ))
          !lo = Literals.read tbl (indexWord8OffAddr# addr (i +# 1#))
        in
        case writeWord8Array# mba (quotInt# i 2#) (uncheckedShiftLWord8# hi 4# `orWord8#` lo) s0 of
          s1 -> loop mba (i +# 2#) (state `orWord8#` orWord8# hi lo) s1
      else
        if isTrue# (state `ltWord8#` 0x10#Word8)
        then case unsafeFreezeByteArray# mba s0 of (# s1, ba #) -> (# s1, Just (mk ba) #)
        else (# s0, Nothing #)


hex_table :: Literals.Table
hex_table =
  [Literals.table|
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    0 1 2 3 4 5 6 7 8 9 _ _ _ _ _ _
    _ A B C D E F _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ A B C D E F _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  |]



-- VALIDATION FSM
--
-- This table is adapted from Bjoern's UTF-8 FSM in C.
--
-- The first part of the table maps bytes to character classes that
-- to reduce the size of the transition table and create bitmasks.
--
-- The second part is a transition table that maps a combination
-- of a state of the automaton and a character class to a state.
--
-- Copyright (c) 2008-2010 Bjoern Hoehrmann <bjoern@hoehrmann.de>
-- Available under an MIT license, with a great explanation of the code at:
--   http://bjoern.hoehrmann.de/utf-8/decoder/dfa/


utf8_fsm :: Literals.FSM
utf8_fsm =
  [fsm|
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9
    7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
    7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
    8 8 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    a 3 3 3 3 3 3 3 3 3 3 3 3 4 3 3
    b 6 6 6 5 8 8 8 8 8 8 8 8 8 8 8
    -------------------------------
    0 1 2 3 5 8 7 1 1 1 4 6
    1 1 1 1 1 1 1 1 1 1 1 1
    1 0 1 1 1 1 1 0 1 0 1 1
    1 2 1 1 1 1 1 2 1 2 1 1
    1 1 1 1 1 1 1 2 1 1 1 1
    1 2 1 1 1 1 1 1 1 2 1 1
    1 1 1 1 1 1 1 3 1 3 1 1
    1 3 1 1 1 1 1 3 1 3 1 1
    1 3 1 1 1 1 1 1 1 1 1 1
  |]

