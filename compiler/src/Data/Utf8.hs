{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
module Data.Utf8
  ( Utf8(..)
  , toString
  , toBuilder
  , fromChunks
  , Chunk(..)
  )
  where


import Data.Binary (Binary(..), Get)
import Data.Binary.Put (putBuilder)
import Data.Binary.Get.Internal (readN)
import Data.Bits ((.&.), shiftR)
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Builder.Internal as B
import Foreign.ForeignPtr (ForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (minusPtr, plusPtr)
import GHC.Exts
  ( Int(I#), Ptr(Ptr), Char(C#)
  , RealWorld
  , ByteArray#, MutableByteArray#
  , isTrue#
  , newByteArray#, unsafeFreezeByteArray#
  , sizeofByteArray#
  , copyAddrToByteArray#, copyByteArrayToAddr#
  , writeWord8Array#,
  )
import GHC.IO
import GHC.ST (ST(ST), runST)
import GHC.Prim
import GHC.Word (Word8(W8#))



-- UTF-8


data Utf8 =
  Utf8 ByteArray#



-- EQUAL


instance Eq Utf8 where
  (==) (Utf8 ba1#) (Utf8 ba2#) =
    let
      !len1# = sizeofByteArray# ba1#
      !len2# = sizeofByteArray# ba2#
    in
    isTrue# (len1# ==# len2#)
    &&
    isTrue# (0# ==# compareByteArrays# ba1# 0# ba2# 0# len1#)



-- COMPARE


instance Ord Utf8 where
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



-- BINARY


instance Binary Utf8 where
  put utf8@(Utf8 ba#) =
    put (I# (sizeofByteArray# ba#)) <> putBuilder (toBuilder utf8)
  get =
    get >>= getUtf8


{-# INLINE getUtf8 #-}
getUtf8 :: Int -> Get Utf8
getUtf8 n =
  if n > 0 then
    readN n (copyFromByteString n)
  else
    return empty


-- TODO add inline?
copyFromByteString :: Int -> B.ByteString -> Utf8
copyFromByteString len (B.PS fptr offset _) =
  unsafeDupablePerformIO
  (
    do  mba <- stToIO (newByteArray len)
        stToIO (copyFromPtr (unsafeForeignPtrToPtr fptr `plusPtr` offset) mba 0 len)
        touchForeignPtr fptr
        stToIO (unsafeFreeze mba)
  )


{-# NOINLINE empty #-}
empty :: Utf8
empty =
  runST (unsafeFreeze =<< newByteArray 0)



-- CHUNKS


data Chunk
  = Slice Int Int -- TODO try UNPACK
  | Escape Word8
  | CodePoint Int


fromChunks :: ForeignPtr Word8 -> [Chunk] -> Utf8
fromChunks fptr chunks =
  unsafeDupablePerformIO
  (
    do  let !len = sum (map chunkToWidth chunks)
        mba <- stToIO (newByteArray len)
        stToIO (writeChunks (unsafeForeignPtrToPtr fptr) mba 0 chunks)
        touchForeignPtr fptr
        stToIO (unsafeFreeze mba)
  )


chunkToWidth :: Chunk -> Int
chunkToWidth chunk =
  case chunk of
    Slice _ len ->
      len

    Escape _ ->
      2

    CodePoint code ->
      if code < 0xFFFF then 6 else 12


writeChunks :: Ptr Word8 -> MBA RealWorld -> Int -> [Chunk] -> ST RealWorld ()
writeChunks ptr mba offset chunks =
  case chunks of
    [] ->
      return ()

    chunk : chunks ->
      case chunk of
        Slice off len ->
          do  copyFromPtr (plusPtr ptr off) mba offset len
              let !newOffset = offset + len
              writeChunks ptr mba newOffset chunks

        Escape word ->
          do  writeWord8 mba offset 0x5C {- \ -}
              writeWord8 mba (offset + 1) word
              let !newOffset = offset + 2
              writeChunks ptr mba newOffset chunks

        CodePoint code ->
          if code < 0xFFFF then
            do  writeCode mba offset code
                let !newOffset = offset + 6
                writeChunks ptr mba newOffset chunks
          else
            do  let (hi,lo) = divMod (code - 0x10000) 0x400
                writeCode mba (offset    ) (hi + 0xD800)
                writeCode mba (offset + 6) (lo + 0xDC00)
                let !newOffset = offset + 12
                writeChunks ptr mba newOffset chunks


-- TODO see if it is faster to writeWord32 a block of hex-as-ascii
writeCode :: MBA RealWorld -> Int -> Int -> ST RealWorld ()
writeCode mba offset code =
  do  writeWord8 mba offset 0x5C {- \ -}
      writeWord8 mba offset 0x75 {- u -}
      writeHex mba (offset + 2) (shiftR code 12)
      writeHex mba (offset + 3) (shiftR code 8)
      writeHex mba (offset + 4) (shiftR code 4)
      writeHex mba (offset + 5) code


writeHex :: MBA RealWorld -> Int -> Int -> ST RealWorld ()
writeHex mba !offset !bits =
  do  let !n = fromIntegral bits .&. 0x0F
      writeWord8 mba offset (if n < 10 then 0x30 + n else 0x37 + n)



-- TO BUILDER


{-# INLINE toBuilder #-}
toBuilder :: Utf8 -> B.Builder
toBuilder =
  \utf8 -> B.builder (toBuilderHelp utf8)


{-# INLINE toBuilderHelp #-}
toBuilderHelp :: Utf8 -> B.BuildStep a -> B.BuildStep a
toBuilderHelp !utf8@(Utf8 ba#) k =
    go 0 (I# (sizeofByteArray# ba#))
  where
    go !offset !end !(B.BufferRange bOffset bEnd) =
      let
        !bLen = minusPtr bEnd bOffset
        !len = end - offset
      in
      if len <= bLen then
        do  stToIO (copyToPtr utf8 offset bOffset len)
            let !br' = B.BufferRange (plusPtr bOffset len) bEnd
            k br'
      else
        do  stToIO (copyToPtr utf8 offset bOffset bLen)
            let !offset' = offset + bLen
            return $ B.bufferFull 1 bEnd (go offset' end)



-- TO STRING


toString :: Utf8 -> String
toString (Utf8 ba#) =
  let
    !len# = sizeofByteArray# ba#
  in
  toStringHelp ba# 0# len#


toStringHelp :: ByteArray# -> Int# -> Int# -> String
toStringHelp ba# offset# len# =
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
    char : toStringHelp ba# newOffset# len#


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



-- PRIMITIVES


data MBA s =
  MBA# (MutableByteArray# s)


newByteArray :: Int -> ST s (MBA s)
newByteArray (I# len#) =
  ST $ \s ->
    case newByteArray# len# s of
      (# s, mba# #) -> (# s, MBA# mba# #)


unsafeFreeze :: MBA s -> ST s Utf8
unsafeFreeze (MBA# mba#) =
  ST $ \s ->
    case unsafeFreezeByteArray# mba# s of
      (# s, ba# #) -> (# s, Utf8 ba# #)


copyFromPtr :: Ptr a -> MBA RealWorld -> Int -> Int -> ST RealWorld ()
copyFromPtr (Ptr src#) (MBA# mba#) (I# dst_off#) (I# len#) =
  ST $ \s ->
    case copyAddrToByteArray# src# mba# dst_off# len# s of
      s -> (# s, () #)


copyToPtr :: Utf8 -> Int -> Ptr a -> Int -> ST RealWorld ()
copyToPtr (Utf8 ba#) (I# offset#) (Ptr mba#) (I# len#) =
    ST $ \s ->
      case copyByteArrayToAddr# ba# offset# mba# len# s of
        s -> (# s, () #)


writeWord8 :: MBA s -> Int -> Word8 -> ST s ()
writeWord8 (MBA# mba#) (I# i#) (W8# w#) =
  ST $ \s ->
    case writeWord8Array# mba# i# w# s of
      s -> (# s, () #)
