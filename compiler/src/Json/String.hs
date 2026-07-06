{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, EmptyDataDecls, ExtendedLiterals, MagicHash, UnboxedTuples #-}
module Json.String
  ( String
  , isEmpty
  --
  , fromAddr
  , fromName
  , fromChars
  , fromSnippet
  , fromComment
  --
  , toChars
  , toBuilder
  )
  where


import Prelude hiding (String)
import qualified Data.ByteString.Builder as B
import qualified Data.Coerce as Coerce
import qualified Data.Name as Name
import qualified Data.Utf8 as Utf8
import GHC.Exts (isTrue#)
import GHC.ForeignPtr (ForeignPtrContents)
import GHC.Prim
import GHC.Int (Int(..))
import GHC.IO (IO(IO))
import System.IO.Unsafe (unsafePerformIO)

import qualified Parse.Primitives as P



-- JSON STRINGS


-- INVARIANT: any Json.String is appropriately escaped already
-- PERF: is this the right representation for Json.String? Maybe ByteString instead?
--
type String =
  Utf8.Utf8 JSON_STRING


data JSON_STRING


isEmpty :: String -> Bool
isEmpty =
  Utf8.isEmpty



-- FROM


fromAddr :: Addr# -> Addr# -> IO String
fromAddr =
  Utf8.fromAddr


fromChars :: [Char] -> String
fromChars =
  Utf8.fromChars


fromSnippet :: P.Snippet -> IO String
fromSnippet =
  Utf8.fromSnippet


fromName :: Name.Name -> String
fromName =
  Coerce.coerce



-- TO


toChars :: String -> [Char]
toChars =
  Utf8.toChars


{-# INLINE toBuilder #-}
toBuilder :: String -> B.Builder
toBuilder =
  Utf8.toBuilder



-- FROM COMMENT


fromComment :: P.Snippet -> String
fromComment (P.Snippet fpc pos end _) =
  unsafePerformIO $
    fromChunks fpc (chompChunks pos end pos [])


chompChunks :: Addr# -> Addr# -> Addr# -> [Chunk] -> [Chunk]
chompChunks pos end start revChunks =
  if P.notLtAddr pos end then
    reverse (addSlice start end revChunks)
  else
    case indexWord8OffAddr# pos 0# of
      0x0A#Word8 {-\n-} -> chompEscape 0x6E#Word8 {-n-} pos end start revChunks
      0x22#Word8 {-"-}  -> chompEscape 0x22#Word8 {-"-} pos end start revChunks
      0x5C#Word8 {-\-}  -> chompEscape 0x5C#Word8 {-\-} pos end start revChunks
      0x0D#Word8 {-\r-} ->
        let
          !newPos = plusAddr# pos 1#
        in
        chompChunks newPos end newPos (addSlice start pos revChunks)

      word ->
        let
          !width = getCharWidth word
          !newPos = plusAddr# pos width
        in
        chompChunks newPos end start revChunks


chompEscape :: Word8# -> Addr# -> Addr# -> Addr# -> [Chunk] -> [Chunk]
chompEscape escape pos end start revChunks =
  let
    !pos1 = plusAddr# pos 1#
  in
  chompChunks pos1 end pos1 (Escape escape : addSlice start pos revChunks)


addSlice :: Addr# -> Addr# -> [Chunk] -> [Chunk]
addSlice start end revChunks =
  if P.eqAddr start end
    then revChunks
    else Slice start (minusAddr# end start) : revChunks


getCharWidth :: Word8# -> Int#
getCharWidth word
  | isTrue# (ltWord8# word 0x80#Word8) = 1#
  | isTrue# (ltWord8# word 0xc0#Word8) = error "Need UTF-8 encoded input. Ran into unrecognized bits."
  | isTrue# (ltWord8# word 0xe0#Word8) = 2#
  | isTrue# (ltWord8# word 0xf0#Word8) = 3#
  | isTrue# (ltWord8# word 0xf8#Word8) = 4#
  | True                               = error "Need UTF-8 encoded input. Ran into unrecognized bits."



-- FROM CHUNKS


data Chunk
  = Slice Addr# Int#
  | Escape Word8#


fromChunks :: ForeignPtrContents -> [Chunk] -> IO String
fromChunks fpc chunks =
  IO $ \s0 ->
    case newByteArray# len          s0 of { (# s1, mba #) ->
    case writeChunks mba 0# chunks  s1 of {    s2         ->
    case touch# fpc                 s2 of {    s3         ->
    case unsafeFreezeByteArray# mba s3 of { (# s4, ba  #) -> (# s4, Utf8.Utf8 ba #) }}}}
  where
    !(I# len) = sum (map chunkToWidth chunks)


chunkToWidth :: Chunk -> Int
chunkToWidth chunk =
  case chunk of
    Slice _ len -> I# len
    Escape _    -> 2


writeChunks :: MutableByteArray# s -> Int# -> [Chunk] -> State# s -> State# s
writeChunks mba off chunks s0 =
  case chunks of
    [] ->
      s0

    chunk : chunks ->
      case chunk of
        Slice pos len ->
          case copyAddrToByteArray# pos mba off len s0 of { s1 ->
            writeChunks mba (off +# len) chunks s1
          }

        Escape word ->
          case writeWord8Array# mba (off      ) 0x5C#Word8 {-\-} s0 of { s1 ->
          case writeWord8Array# mba (off +# 1#) word             s1 of { s2 ->
            writeChunks mba (off +# 2#) chunks s2
          }}
