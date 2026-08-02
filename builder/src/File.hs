{-# LANGUAGE BangPatterns #-}
module File
  ( Time
  , getTime
  , zeroTime
  --
  , Writer
  , withWriter
  --
  , writeBuilder
  , writeBuilder_
  , writeUtf8
  , readUtf8
  --
  , readBytes
  , writeBytes
  , writeBytes_
  --
  , exists
  , remove
  , removeDir
  --
  , eTime, dTime
  , ePath, dPath
  )
  where


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Exception (catch)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Builder as B
import qualified Data.List as List
import Data.Foldable (traverse_)
import qualified Data.Time.Calendar as Time
import qualified Data.Time.Clock as Time
import qualified Foreign.ForeignPtr as FPtr
import GHC.Int (Int64)
import GHC.IO.Exception (IOException, IOErrorType(InvalidArgument))
import GHC.Word (Word32)
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import qualified System.IO as IO
import System.IO.Error (ioeGetErrorType, annotateIOError, modifyIOError)

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified String as S



-- TIME
--
-- PERF is it worthwhile to avoid going through UTCTime?


data Time =
  Time
    { _secs  :: Int64
    , _nanos :: Word32
    }
  deriving (Eq, Ord)


getTime :: FilePath -> IO Time
getTime path =
  do  (Time.UTCTime (Time.ModifiedJulianDay d) t) <- Dir.getModificationTime path
      let (secs, picos) = fromIntegral (Time.diffTimeToPicoseconds t) `divMod` 1000000000000
      return $ Time (86400 * (fromIntegral d - 40587) + secs) (fromIntegral picos `div` 1000)


zeroTime :: Time
zeroTime =
  Time 0 0



-- FILE WRITER
--
-- This allows for background writes that are guaranteed to complete even if
-- they take a bit longer than the actual program itself. Background writes are
-- indicated with a trailing underscore.


newtype Writer t =
  Writer (MVar [MVar ()])


withWriter :: (Writer t -> IO a) -> IO a
withWriter callback =
  do  workList <- newMVar []
      result <- callback (Writer workList)
      mvars <- takeMVar workList
      traverse_ takeMVar mvars
      return result



-- WRITE BUILDER


writeBuilder :: Writer t -> FilePath -> B.Builder -> IO ()
writeBuilder _ path builder =
  do  let dir = FP.dropFileName path
      Dir.createDirectoryIfMissing True dir
      IO.withBinaryFile path IO.WriteMode $ \handle ->
        do  IO.hSetBuffering handle (IO.BlockBuffering Nothing)
            B.hPutBuilder handle builder


writeBuilder_ :: Writer t -> FilePath -> B.Builder -> IO ()
writeBuilder_ writer@(Writer workList) path builder =
  do  mvar <- newEmptyMVar
      _ <- forkIO (writeBuilder writer path builder >> putMVar mvar ())
      oldWork <- takeMVar workList
      let !newWork = mvar:oldWork
      putMVar workList newWork



-- WRITE UTF-8


writeUtf8 :: Writer t -> FilePath -> BS.ByteString -> IO ()
writeUtf8 _ path content =
  IO.withFile path IO.WriteMode $ \handle ->
    do  IO.hSetEncoding handle IO.utf8
        BS.hPut handle content



-- READ UTF-8


readUtf8 :: FilePath -> IO BS.ByteString
readUtf8 path =
  IO.withFile path IO.ReadMode $ \handle ->
    modifyIOError (encodingError path) $
    do  IO.hSetEncoding handle IO.utf8
        fileSize <- catch (IO.hFileSize handle) useZeroIfNotRegularFile
        let readSize = max 0 (fromIntegral fileSize) + 1
        hGetContentsSizeHint handle readSize (max 255 readSize)


useZeroIfNotRegularFile :: IOException -> IO Integer
useZeroIfNotRegularFile _ =
  return 0


hGetContentsSizeHint :: IO.Handle -> Int -> Int -> IO BS.ByteString
hGetContentsSizeHint handle =
    readChunks []
  where
    readChunks chunks readSize incrementSize =
      do  fp <- BS.mallocByteString readSize
          readCount <- FPtr.withForeignPtr fp $ \buf -> IO.hGetBuf handle buf readSize
          let chunk = BS.BS fp readCount
          if readCount < readSize && readSize > 0
            then return $! BS.concat (List.reverse (chunk:chunks))
            else readChunks (chunk:chunks) incrementSize (min 32752 (readSize + incrementSize))


encodingError :: FilePath -> IOError -> IOError
encodingError path ioErr =
  case ioeGetErrorType ioErr of
    InvalidArgument ->
      annotateIOError
        (userError "Bad encoding; the file must be valid UTF-8")
        ""
        Nothing
        (Just path)

    _ ->
      ioErr



-- READ BYTES


readBytes :: D.Decoder a -> FilePath -> IO (Maybe a)
readBytes decoder path =
  do  exists_ <- Dir.doesFileExist path
      if exists_
        then
          do  result <- D.fromFile decoder path
              case result of
                Right a -> return $ Just a
                Left  t ->
                  do  IO.hPutStrLn IO.stderr $ unlines $
                        [ "+-------------------------------------------------------------------------------"
                        , "| Corrupt File: " ++ path
                        , "| Error when expecting: " ++ t
                        , "|"
                        , "| Please report this to https://github.com/elm/compiler/issues"
                        , "| Trying to continue anyway."
                        , "+-------------------------------------------------------------------------------"
                        ]
                      return Nothing
        else
          return Nothing





-- WRITE BYTES


writeBytes :: Writer t -> FilePath -> (a -> B.Builder) -> a -> IO ()
writeBytes writer path func value =
  writeBuilder writer path (func value)


writeBytes_ :: Writer t -> FilePath -> (a -> B.Builder) -> a -> IO ()
writeBytes_ writer@(Writer workList) path func value =
  do  mvar <- newEmptyMVar
      _ <- forkIO (writeBuilder writer path (func value) >> putMVar mvar ())
      oldWork <- takeMVar workList
      let !newWork = mvar:oldWork
      putMVar workList newWork



-- EXISTS


exists :: FilePath -> IO Bool
exists path =
  Dir.doesFileExist path



-- REMOVE FILES


remove :: FilePath -> IO ()
remove path =
  do  exists_ <- Dir.doesFileExist path
      if exists_
        then Dir.removeFile path
        else return ()


removeDir :: FilePath -> IO ()
removeDir path =
  do  exists_ <- Dir.doesDirectoryExist path
      if exists_
        then Dir.removeDirectoryRecursive path
        else return ()


--------------------------------------------------------------------------------
-- BINARY FORMAT ---------------------------------------------------------------
--------------------------------------------------------------------------------



-- BINARY ENCODE/DECODE TIME


eTime :: Time -> E.Builder
eTime (Time secs nanos) =
  E.i64 secs <> E.u32 nanos


dTime :: D.Decoder Time
dTime =
  D.map2 Time D.i64 D.u32



-- BINARY ENCODE/DECODE PATH
--
-- PERF figure out better way to store FilePaths as dense UTF-8


ePath :: FilePath -> E.Builder
ePath path =
  E.string16 (S.fromChars path)


dPath :: D.Decoder FilePath
dPath =
  S.toChars <$> D.string16


