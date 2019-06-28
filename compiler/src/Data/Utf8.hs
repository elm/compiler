{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, FlexibleInstances, MagicHash, UnboxedTuples #-}
module Data.Utf8
  ( Utf8(..)
  , isEmpty
  , empty
  , size
  , contains
  , startsWith
  , startsWithChar
  , endsWithWord8
  , split
  , join
  --
  , getUnder256
  , putUnder256
  --
  , getVeryLong
  , putVeryLong
  --
  , toChars
  , toBuilder
  , toEscapedBuilder
  --
  , fromPtr
  , fromSnippet
  , fromChars
  --
  , MBA
  , newByteArray
  , copyFromPtr
  , writeWord8
  , freeze
  )
  where


import Prelude hiding (String, all, any, concat)
import Data.Binary (Get, get, getWord8, Put, put, putWord8)
import Data.Binary.Put (putBuilder)
import Data.Binary.Get.Internal (readN)
import Data.Bits ((.&.), shiftR)
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Builder.Internal as B
import qualified Data.Char as Char
import qualified Data.List as List
import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (minusPtr, plusPtr)
import GHC.Exts
  ( Int(I#), Ptr(Ptr), Char(C#)
  , RealWorld
  , ByteArray#, MutableByteArray#
  , isTrue#
  , newByteArray#
  , unsafeFreezeByteArray#
  , sizeofByteArray#
  , copyByteArray#
  , copyAddrToByteArray#
  , copyByteArrayToAddr#
  , writeWord8Array#
  )
import GHC.IO
import GHC.ST (ST(ST), runST)
import GHC.Prim
import GHC.Word (Word8(W8#))

import qualified Parse.Primitives as P



-- UTF-8


data Utf8 tipe =
  Utf8 ByteArray#



-- EMPTY


{-# NOINLINE empty #-}
empty :: Utf8 t
empty =
  runST (freeze =<< newByteArray 0)


isEmpty :: Utf8 t -> Bool
isEmpty (Utf8 ba#) =
  isTrue# (sizeofByteArray# ba# ==# 0#)



-- SIZE


size :: Utf8 t -> Int
size (Utf8 ba#) =
  I# (sizeofByteArray# ba#)



-- CONTAINS


contains :: Word8 -> Utf8 t -> Bool
contains (W8# word#) (Utf8 ba#) =
  containsHelp word# ba# 0# (sizeofByteArray# ba#)


containsHelp :: Word# -> ByteArray# -> Int# -> Int# -> Bool
containsHelp word# ba# !offset# len# =
  if isTrue# (offset# <# len#) then
    if isTrue# (eqWord# word# (indexWord8Array# ba# offset#))
      then True
      else containsHelp word# ba# (offset# +# 1#) len#
  else
    False



-- STARTS WITH


{-# INLINE startsWith #-}
startsWith :: Utf8 t -> Utf8 t -> Bool
startsWith (Utf8 ba1#) (Utf8 ba2#) =
  let
    !len1# = sizeofByteArray# ba1#
    !len2# = sizeofByteArray# ba2#
  in
  isTrue# (len1# <=# len2#)
  &&
  isTrue# (0# ==# compareByteArrays# ba1# 0# ba2# 0# len1#)



-- STARTS WITH CHAR


startsWithChar :: (Char -> Bool) -> Utf8 t -> Bool
startsWithChar isGood bytes@(Utf8 ba#) =
  if isEmpty bytes then
    False
  else
    let
      !w# = indexWord8Array# ba# 0#
      !char
        | isTrue# (ltWord# w# 0xC0##) = C# (chr# (word2Int# w#))
        | isTrue# (ltWord# w# 0xE0##) = chr2 ba# 0# w#
        | isTrue# (ltWord# w# 0xF0##) = chr3 ba# 0# w#
        | True                        = chr4 ba# 0# w#
    in
    isGood char



-- ENDS WITH WORD


endsWithWord8 :: Word8 -> Utf8 t -> Bool
endsWithWord8 (W8# w#) (Utf8 ba#) =
  let len# = sizeofByteArray# ba# in
  isTrue# (len# ># 0#)
  &&
  isTrue# (eqWord# w# (indexWord8Array# ba# (len# -# 1#)))



-- SPLIT


split :: Word8 -> Utf8 t -> [Utf8 t]
split (W8# divider#) str@(Utf8 ba#) =
  splitHelp str 0 (findDividers divider# ba# 0# (sizeofByteArray# ba#) [])


splitHelp :: Utf8 t -> Int -> [Int] -> [Utf8 t]
splitHelp str start offsets =
  case offsets of
    [] ->
      [ unsafeSlice str start (size str) ]

    offset : offsets ->
      unsafeSlice str start offset : splitHelp str (offset + 1) offsets


findDividers :: Word# -> ByteArray# -> Int# -> Int# -> [Int] -> [Int]
findDividers divider# ba# !offset# len# revOffsets =
  if isTrue# (offset# <# len#) then
    findDividers divider# ba# (offset# +# 1#) len# $
      if isTrue# (eqWord# divider# (indexWord8Array# ba# offset#))
      then I# offset# : revOffsets
      else revOffsets
  else
    reverse revOffsets


unsafeSlice :: Utf8 t -> Int -> Int -> Utf8 t
unsafeSlice str start end =
  let !len = end - start in
  if len == 0 then
    empty
  else
    runST $
    do  mba <- newByteArray len
        copy str start mba 0 len
        freeze mba



-- JOIN


join :: Word8 -> [Utf8 t] -> Utf8 t
join sep strings =
  case strings of
    [] ->
      empty

    str:strs ->
      runST $
      do  let !len = List.foldl' (\w s -> w + 1 + size s) (size str) strs
          mba <- newByteArray len
          joinHelp sep mba 0 str strs
          freeze mba


joinHelp :: Word8 -> MBA s -> Int -> Utf8 t -> [Utf8 t] -> ST s ()
joinHelp sep mba offset str strings =
  let
    !len = size str
  in
  case strings of
    [] ->
      copy str 0 mba offset len

    s:ss ->
      do  copy str 0 mba offset len
          let !dotOffset = offset + len
          writeWord8 mba dotOffset sep
          let !newOffset = dotOffset + 1
          joinHelp sep mba newOffset s ss



-- EQUAL


instance Eq (Utf8 t) where
  (==) (Utf8 ba1#) (Utf8 ba2#) =
    let
      !len1# = sizeofByteArray# ba1#
      !len2# = sizeofByteArray# ba2#
    in
    isTrue# (len1# ==# len2#)
    &&
    isTrue# (0# ==# compareByteArrays# ba1# 0# ba2# 0# len1#)



-- COMPARE


instance Ord (Utf8 t) where
  compare (Utf8 ba1#) (Utf8 ba2#) =
    let
      !len1# = sizeofByteArray# ba1#
      !len2# = sizeofByteArray# ba2#
      !len#  = if isTrue# (len1# <# len2#) then len1# else len2#
      !cmp#  = compareByteArrays# ba1# 0# ba2# 0# len#
    in
    case () of
      _ | isTrue# (cmp# <# 0#)     -> LT
        | isTrue# (cmp# ># 0#)     -> GT
        | isTrue# (len1# <# len2#) -> LT
        | isTrue# (len1# ># len2#) -> GT
        | True                     -> EQ



-- FROM STRING


fromChars :: [Char] -> Utf8 t
fromChars chars =
  runST
  (
    do  mba <- newByteArray (sum (map getWidth chars))
        writeChars mba 0 chars
  )


writeChars :: MBA s -> Int -> [Char] -> ST s (Utf8 t)
writeChars !mba !offset chars =
  case chars of
    [] ->
      freeze mba

    char : chars
      | n < 0x80 ->
          do  writeWord8 mba (offset    ) (fromIntegral n)
              writeChars mba (offset + 1) chars

      | n < 0x800 ->
          do  writeWord8 mba (offset    ) (fromIntegral ((shiftR n 6         ) + 0xC0))
              writeWord8 mba (offset + 1) (fromIntegral ((       n   .&. 0x3F) + 0x80))
              writeChars mba (offset + 2) chars

      | n < 0x10000 ->
          do  writeWord8 mba (offset    ) (fromIntegral ((shiftR n 12         ) + 0xE0))
              writeWord8 mba (offset + 1) (fromIntegral ((shiftR n  6 .&. 0x3F) + 0x80))
              writeWord8 mba (offset + 2) (fromIntegral ((       n    .&. 0x3F) + 0x80))
              writeChars mba (offset + 3) chars

      | otherwise ->
          do  writeWord8 mba (offset    ) (fromIntegral ((shiftR n 18         ) + 0xF0))
              writeWord8 mba (offset + 1) (fromIntegral ((shiftR n 12 .&. 0x3F) + 0x80))
              writeWord8 mba (offset + 2) (fromIntegral ((shiftR n  6 .&. 0x3F) + 0x80))
              writeWord8 mba (offset + 3) (fromIntegral ((       n    .&. 0x3F) + 0x80))
              writeChars mba (offset + 4) chars

      where
        n = Char.ord char


{-# INLINE getWidth #-}
getWidth :: Char -> Int
getWidth char
  | code < 0x80    = 1
  | code < 0x800   = 2
  | code < 0x10000 = 3
  | otherwise      = 4
  where
    code = Char.ord char



-- TO CHARS


toChars :: Utf8 t -> [Char]
toChars (Utf8 ba#) =
  toCharsHelp ba# 0# (sizeofByteArray# ba#)


toCharsHelp :: ByteArray# -> Int# -> Int# -> [Char]
toCharsHelp ba# offset# len# =
  if isTrue# (offset# >=# len#) then
    []
  else
    let
      !w# = indexWord8Array# ba# offset#
      !(# char, width# #)
        | isTrue# (ltWord# w# 0xC0##) = (# C# (chr# (word2Int# w#)), 1# #)
        | isTrue# (ltWord# w# 0xE0##) = (# chr2 ba# offset# w#, 2# #)
        | isTrue# (ltWord# w# 0xF0##) = (# chr3 ba# offset# w#, 3# #)
        | True                        = (# chr4 ba# offset# w#, 4# #)

      !newOffset# = offset# +# width#
    in
    char : toCharsHelp ba# newOffset# len#


{-# INLINE chr2 #-}
chr2 :: ByteArray# -> Int# -> Word# -> Char
chr2 ba# offset# firstWord# =
  let
    !i1# = word2Int# firstWord#
    !i2# = word2Int# (indexWord8Array# ba# (offset# +# 1#))
    !c1# = uncheckedIShiftL# (i1# -# 0xC0#) 6#
    !c2# = i2# -# 0x80#
  in
  C# (chr# (c1# +# c2#))


{-# INLINE chr3 #-}
chr3 :: ByteArray# -> Int# -> Word# -> Char
chr3 ba# offset# firstWord# =
  let
    !i1# = word2Int# firstWord#
    !i2# = word2Int# (indexWord8Array# ba# (offset# +# 1#))
    !i3# = word2Int# (indexWord8Array# ba# (offset# +# 2#))
    !c1# = uncheckedIShiftL# (i1# -# 0xE0#) 12#
    !c2# = uncheckedIShiftL# (i2# -# 0x80#) 6#
    !c3# = i3# -# 0x80#
  in
  C# (chr# (c1# +# c2# +# c3#))


{-# INLINE chr4 #-}
chr4 :: ByteArray# -> Int# -> Word# -> Char
chr4 ba# offset# firstWord# =
  let
    !i1# = word2Int# firstWord#
    !i2# = word2Int# (indexWord8Array# ba# (offset# +# 1#))
    !i3# = word2Int# (indexWord8Array# ba# (offset# +# 2#))
    !i4# = word2Int# (indexWord8Array# ba# (offset# +# 3#))
    !c1# = uncheckedIShiftL# (i1# -# 0xF0#) 18#
    !c2# = uncheckedIShiftL# (i2# -# 0x80#) 12#
    !c3# = uncheckedIShiftL# (i3# -# 0x80#) 6#
    !c4# = i4# -# 0x80#
  in
  C# (chr# (c1# +# c2# +# c3# +# c4#))



-- TO BUILDER


{-# INLINE toBuilder #-}
toBuilder :: Utf8 t -> B.Builder
toBuilder =
  \bytes -> B.builder (toBuilderHelp bytes)


{-# INLINE toBuilderHelp #-}
toBuilderHelp :: Utf8 t -> B.BuildStep a -> B.BuildStep a
toBuilderHelp !bytes@(Utf8 ba#) k =
    go 0 (I# (sizeofByteArray# ba#))
  where
    go !offset !end !(B.BufferRange bOffset bEnd) =
      let
        !bLen = minusPtr bEnd bOffset
        !len = end - offset
      in
      if len <= bLen then
        do  copyToPtr bytes offset bOffset len
            let !br' = B.BufferRange (plusPtr bOffset len) bEnd
            k br'
      else
        do  copyToPtr bytes offset bOffset bLen
            let !offset' = offset + bLen
            return $ B.bufferFull 1 bEnd (go offset' end)



-- TO ESCAPED BUILDER


{-# INLINE toEscapedBuilder #-}
toEscapedBuilder :: Word8 -> Word8 -> Utf8 t -> B.Builder
toEscapedBuilder before after =
  \name -> B.builder (toEscapedBuilderHelp before after name)


{-# INLINE toEscapedBuilderHelp #-}
toEscapedBuilderHelp :: Word8 -> Word8 -> Utf8 t -> B.BuildStep a -> B.BuildStep a
toEscapedBuilderHelp before after !name@(Utf8 ba#) k =
    go 0 (I# (sizeofByteArray# ba#))
  where
    go !offset !len !(B.BufferRange bOffset bEnd) =
      let
        !bLen = minusPtr bEnd bOffset
      in
      if len <= bLen then
        do  -- PERF test if writing word-by-word is faster
            copyToPtr name offset bOffset len
            escape before after bOffset name offset len 0
            let !newBufferRange = B.BufferRange (plusPtr bOffset len) bEnd
            k newBufferRange
      else
        do  copyToPtr name offset bOffset bLen
            escape before after bOffset name offset bLen 0
            let !newOffset = offset + bLen
            let !newLength = len - bLen
            return $ B.bufferFull 1 bEnd (go newOffset newLength)


escape :: Word8 -> Word8 -> Ptr a -> Utf8 t -> Int -> Int -> Int -> IO ()
escape before@(W8# before#) after ptr name@(Utf8 ba#) offset@(I# offset#) len@(I# len#) i@(I# i#) =
  if isTrue# (i# <# len#) then
    if isTrue# (eqWord# before# (indexWord8Array# ba# (offset# +# i#)))
    then
      do  writeWordToPtr ptr i after
          escape before after ptr name offset len (i + 1)
    else
      do  escape before after ptr name offset len (i + 1)

  else
    return ()



-- FROM PTR


fromPtr :: Ptr Word8 -> Ptr Word8 -> Utf8 t
fromPtr pos end =
  unsafeDupablePerformIO (stToIO (
    do  let !len = minusPtr end pos
        mba <- newByteArray len
        copyFromPtr pos mba 0 len
        freeze mba
  ))



-- FROM SNIPPET


fromSnippet :: P.Snippet -> Utf8 t
fromSnippet (P.Snippet fptr off len _ _) =
  unsafeDupablePerformIO (stToIO (
    do  mba <- newByteArray len
        let !pos = plusPtr (unsafeForeignPtrToPtr fptr) off
        copyFromPtr pos mba 0 len
        freeze mba
  ))



-- BINARY


putUnder256 :: Utf8 t -> Put
putUnder256 bytes =
  do  putWord8 (fromIntegral (size bytes))
      putBuilder (toBuilder bytes)


getUnder256 :: Get (Utf8 t)
getUnder256 =
  do  word <- getWord8
      let !n = fromIntegral word
      readN n (copyFromByteString n)


putVeryLong :: Utf8 t -> Put
putVeryLong bytes =
  do  put (size bytes)
      putBuilder (toBuilder bytes)


getVeryLong :: Get (Utf8 t)
getVeryLong =
  do  n <- get
      if n > 0
        then readN n (copyFromByteString n)
        else return empty



-- COPY FROM BYTESTRING


{-# INLINE copyFromByteString #-}
copyFromByteString :: Int -> B.ByteString -> Utf8 t
copyFromByteString len (B.PS fptr offset _) =
  unsafeDupablePerformIO
  (
    do  mba <- stToIO (newByteArray len)
        stToIO (copyFromPtr (unsafeForeignPtrToPtr fptr `plusPtr` offset) mba 0 len)
        touchForeignPtr fptr
        stToIO (freeze mba)
  )



-- PRIMITIVES


data MBA s =
  MBA# (MutableByteArray# s)


newByteArray :: Int -> ST s (MBA s) -- PERF see if newPinnedByteArray for len > 256 is positive
newByteArray (I# len#) =
  ST $ \s ->
    case newByteArray# len# s of
      (# s, mba# #) -> (# s, MBA# mba# #)


freeze :: MBA s -> ST s (Utf8 t)
freeze (MBA# mba#) =
  ST $ \s ->
    case unsafeFreezeByteArray# mba# s of
      (# s, ba# #) -> (# s, Utf8 ba# #)


copy :: Utf8 t -> Int -> MBA s -> Int -> Int -> ST s ()
copy (Utf8 ba#) (I# offset#) (MBA# mba#) (I# i#) (I# len#) =
  ST $ \s ->
    case copyByteArray# ba# offset# mba# i# len# s of
      s -> (# s, () #)


copyFromPtr :: Ptr a -> MBA RealWorld -> Int -> Int -> ST RealWorld ()
copyFromPtr (Ptr src#) (MBA# mba#) (I# offset#) (I# len#) =
  ST $ \s ->
    case copyAddrToByteArray# src# mba# offset# len# s of
      s -> (# s, () #)


copyToPtr :: Utf8 t -> Int -> Ptr a -> Int -> IO ()
copyToPtr (Utf8 ba#) (I# offset#) (Ptr mba#) (I# len#) =
  IO $ \s ->
    case copyByteArrayToAddr# ba# offset# mba# len# s of
      s -> (# s, () #)


{-# INLINE writeWord8 #-}
writeWord8 :: MBA s -> Int -> Word8 -> ST s ()
writeWord8 (MBA# mba#) (I# offset#) (W8# w#) =
  ST $ \s ->
    case writeWord8Array# mba# offset# w# s of
      s -> (# s, () #)


{-# INLINE writeWordToPtr #-}
writeWordToPtr :: Ptr a -> Int -> Word8 -> IO ()
writeWordToPtr (Ptr addr#) (I# offset#) (W8# word#) =
  IO $ \s ->
    case writeWord8OffAddr# addr# offset# word# s of
      s -> (# s, () #)
