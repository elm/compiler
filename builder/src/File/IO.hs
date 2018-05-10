module File.IO
  ( writeBinary, readBinary
  , writeUtf8, readUtf8
  , writeBuilder
  , Writer
  , put
  , putByteString
  , putBuilder
  , putFile
  , exists
  , remove, removeDir
  , find
  , andM
  )
  where


import Control.Exception (catch)
import Control.Monad.Trans (liftIO)
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as B (defaultChunkSize)
import qualified Foreign.ForeignPtr as FPtr
import GHC.IO.Exception (IOException, IOErrorType(InvalidArgument))
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.FilePath as FP
import qualified System.IO as IO
import System.IO.Error (ioeGetErrorType, annotateIOError, modifyIOError)

import qualified Elm.PerUserCache as PerUserCache
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Assets as E
import qualified Reporting.Task as Task



-- BINARY


writeBinary :: (Binary.Binary a) => FilePath -> a -> Task.Task ()
writeBinary path value =
  liftIO $
    do  let dir = FP.dropFileName path
        Dir.createDirectoryIfMissing True dir
        Binary.encodeFile path value


readBinary :: (Binary.Binary a) => FilePath -> Task.Task a
readBinary path =
  do  exists_ <- liftIO (Dir.doesFileExist path)
      if not exists_
        then throwCorruptBinary path
        else
          do  result <- liftIO (Binary.decodeFileOrFail path)
              case result of
                Left _ ->
                  throwCorruptBinary path

                Right value ->
                  return value


throwCorruptBinary :: FilePath -> Task.Task a
throwCorruptBinary filePath =
  do  elmHome <- liftIO PerUserCache.getElmHome
      Task.throw (Exit.Assets (E.CorruptBinary elmHome filePath))



-- WRITE UTF-8


writeUtf8 :: FilePath -> BS.ByteString -> IO ()
writeUtf8 filePath content =
  withUtf8 filePath IO.WriteMode $ \handle ->
    BS.hPut handle content


withUtf8 :: FilePath -> IO.IOMode -> (IO.Handle -> IO a) -> IO a
withUtf8 filePath mode callback =
  IO.withFile filePath mode $ \handle ->
    do  IO.hSetEncoding handle IO.utf8
        callback handle



-- READ UTF-8


readUtf8 :: FilePath -> IO BS.ByteString
readUtf8 filePath =
  withUtf8 filePath IO.ReadMode $ \handle ->
    modifyIOError (encodingError filePath) $
      do  fileSize <- catch (IO.hFileSize handle) useZeroIfNotRegularFile
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
          let chunk = BS.PS fp 0 readCount
          if readCount < readSize && readSize > 0
            then return $! BS.concat (reverse (chunk:chunks))
            else readChunks (chunk:chunks) incrementSize (min 32752 (readSize + incrementSize))


encodingError :: FilePath -> IOError -> IOError
encodingError filePath ioErr =
  case ioeGetErrorType ioErr of
    InvalidArgument ->
      annotateIOError
        (userError "Bad encoding; the file must be valid UTF-8")
        ""
        Nothing
        (Just filePath)

    _ ->
      ioErr



-- WRITE BUILDER


writeBuilder :: FilePath -> B.Builder -> IO ()
writeBuilder path builder =
  IO.withBinaryFile path IO.WriteMode $ \handle ->
    do  IO.hSetBuffering handle (IO.BlockBuffering Nothing)
        B.hPutBuilder handle builder



-- WRITER


newtype Writer =
  Writer (IO.Handle -> IO ())


put :: FilePath -> Writer -> IO ()
put path (Writer callback) =
  IO.withBinaryFile path IO.WriteMode callback


putByteString :: BS.ByteString -> Writer
putByteString chunk =
  Writer $ \handle ->
    BS.hPut handle chunk


putBuilder :: B.Builder -> Writer
putBuilder builder =
  Writer $ \handle ->
    B.hPutBuilder handle builder


putFile :: FilePath -> Writer
putFile path =
  Writer $ \sink ->
    IO.withBinaryFile path IO.ReadMode $ \source ->
      putHelp source sink


putHelp :: IO.Handle -> IO.Handle -> IO ()
putHelp source sink =
  do  chunk <- BS.hGet source B.defaultChunkSize
      if BS.null chunk
        then return ()
        else
          do  BS.hPut sink chunk
              putHelp source sink



-- EXISTS


exists :: FilePath -> Task.Task_ e Bool
exists filePath =
  liftIO $ Dir.doesFileExist filePath



-- REMOVE FILES


remove :: FilePath -> Task.Task_ e ()
remove filePath =
  liftIO $
    do  exists_ <- Dir.doesFileExist filePath
        if exists_
          then Dir.removeFile filePath
          else return ()


removeDir :: FilePath -> IO ()
removeDir path =
  do  exists_ <- Dir.doesDirectoryExist path
      if exists_
        then Dir.removeDirectoryRecursive path
        else return ()



-- FIND FILES


find :: FilePath -> IO (Maybe FilePath)
find name =
  do  subDir <- Dir.getCurrentDirectory
      findHelp name (FP.splitDirectories subDir)


findHelp :: FilePath -> [String] -> IO (Maybe FilePath)
findHelp name dirs =
  if null dirs then
    return Nothing

  else
    do  exists_ <- Dir.doesFileExist (FP.joinPath dirs </> name)
        if exists_
          then return (Just (FP.joinPath dirs))
          else findHelp name (init dirs)



-- HELPER


andM :: (Monad m) => [m Bool] -> m Bool
andM checks =
  case checks of
    [] ->
      return True

    check : otherChecks ->
      do  bool <- check
          if bool then andM otherChecks else return False
