{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, EmptyDataDecls, ExtendedLiterals, FlexibleInstances,
MagicHash, UnboxedTuples
#-}
module Elm.String
  ( String
  , toChars
  , toBuilder
  , Chunk(..)
  , fromChunks
  )
  where


import Prelude hiding (String)
import Data.Binary (Binary, get, put)
import qualified Data.ByteString.Builder as B
import qualified Data.Utf8 as Utf8
import Data.Utf8 (MBA, newByteArray, copyFromAddr, freeze, writeWord8)
import GHC.Exts (isTrue#)
import GHC.Int (Int(..))
import GHC.IO (stToIO, unsafeDupablePerformIO)
import GHC.Prim
import GHC.ST (ST)
import GHC.Word (Word8(..))



-- STRINGS


type String =
  Utf8.Utf8 ELM_STRING


data ELM_STRING



-- HELPERS


toChars :: String -> [Char]
toChars =
  Utf8.toChars


{-# INLINE toBuilder #-}
toBuilder :: String -> B.Builder
toBuilder =
  Utf8.toBuilder



-- FROM CHUNKS


data Chunk
  = Slice Addr# Int#
  | Escape Word8
  | CodePoint Word#


fromChunks :: [Chunk] -> String
fromChunks chunks =
  unsafeDupablePerformIO (stToIO (
    do  let !len = sum (map chunkToWidth chunks)
        mba <- newByteArray len
        writeChunks mba 0 chunks
        freeze mba
  ))


chunkToWidth :: Chunk -> Int
chunkToWidth chunk =
  case chunk of
    Slice _ len -> I# len
    Escape _    -> 2
    CodePoint c -> if isTrue# (ltWord# c 0xFFFF##) then 6 else 12


writeChunks :: MBA RealWorld -> Int -> [Chunk] -> ST RealWorld ()
writeChunks mba offset chunks =
  case chunks of
    [] ->
      return ()

    chunk : chunks ->
      case chunk of
        Slice addr len ->
          do  copyFromAddr addr mba offset len
              let !newOffset = offset + I# len
              writeChunks mba newOffset chunks

        Escape word ->
          do  writeWord8 mba offset 0x5C {- \ -}
              writeWord8 mba (offset + 1) word
              let !newOffset = offset + 2
              writeChunks mba newOffset chunks

        CodePoint code ->
          if isTrue# (ltWord# code 0xFFFF##) then
            do  writeCode mba offset code
                let !newOffset = offset + 6
                writeChunks mba newOffset chunks
          else
            do  let !(# hi, lo #) = quotRemWord# (minusWord# code 0x10000##) 0x400##
                writeCode mba (offset    ) (plusWord# hi 0xD800##)
                writeCode mba (offset + 6) (plusWord# lo 0xDC00##)
                let !newOffset = offset + 12
                writeChunks mba newOffset chunks


writeCode :: MBA RealWorld -> Int -> Word# -> ST RealWorld ()
writeCode mba offset code =
  do  writeWord8 mba offset 0x5C {- \ -}
      writeWord8 mba (offset + 1) 0x75 {- u -}
      writeHex mba (offset + 2) (uncheckedShiftRL# code 12#)
      writeHex mba (offset + 3) (uncheckedShiftRL# code 8#)
      writeHex mba (offset + 4) (uncheckedShiftRL# code 4#)
      writeHex mba (offset + 5) code


writeHex :: MBA RealWorld -> Int -> Word# -> ST RealWorld ()
writeHex mba !offset bits =
  do  let !n = wordToWord8# (and# bits 0x0F##)
      writeWord8 mba offset (W8# (if isTrue# (ltWord8# n 10#Word8) then plusWord8# 0x30#Word8 n else plusWord8# 0x37#Word8 n))



-- BINARY


instance Binary (Utf8.Utf8 ELM_STRING) where
  get = Utf8.getVeryLong
  put = Utf8.putVeryLong
