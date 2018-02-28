{-# OPTIONS_GHC -Wall #-}
module File.Hash
  ( Hasher
  , put
  , Digest
  , toString
  , putByteString
  , putBuilder
  , putFile
  )
  where


import Prelude hiding (appendFile)
import Control.Monad (foldM)
import qualified Control.Monad.State as State
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS (defaultChunkSize)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Digest.Pure.SHA as SHA
import qualified System.IO as IO



-- HASHER


type Hasher = State.StateT State IO ()


type Digest = SHA.Digest SHA.SHA1State


put :: FilePath -> Hasher -> IO Digest
put path hasher =
  IO.withBinaryFile path IO.WriteMode $ \handle ->
    do  let state = State handle 0 SHA.sha1Incremental
        (State _ len decoder) <- State.execStateT hasher state
        return (SHA.completeSha1Incremental decoder len)


toString :: Digest -> String
toString =
  SHA.showDigest



-- HASH STATE


data State =
  State
    { _handle :: !IO.Handle
    , _length :: !Int
    , _decoder :: !(Binary.Decoder SHA.SHA1State)
    }



-- PUBLIC API


putByteString :: BS.ByteString -> Hasher
putByteString chunk =
  do  state <- State.get
      State.put =<< State.liftIO (appendByteString state chunk)


putBuilder :: BS.Builder -> Hasher
putBuilder builder =
  do  state <- State.get
      State.put =<< State.liftIO (appendBuilder builder state)


putFile :: FilePath -> Hasher
putFile path =
  do  state <- State.get
      State.put =<< State.liftIO (appendFile path state)



-- PRIMITIVES


appendByteString :: State -> BS.ByteString -> IO State
appendByteString (State handle len decoder) chunk =
  do  BS.hPut handle chunk
      return $ State handle (len + BS.length chunk) (Binary.pushChunk decoder chunk)


appendBuilder :: BS.Builder -> State -> IO State
appendBuilder builder state =
  foldM appendByteString state $
    LBS.toChunks (BS.toLazyByteString builder)


appendFile :: FilePath -> State -> IO State
appendFile path state =
  IO.withBinaryFile path IO.ReadMode $ \handle ->
    appendHelp handle state


appendHelp :: IO.Handle -> State -> IO State
appendHelp handle state =
  do  chunk <- BS.hGet handle BS.defaultChunkSize
      if BS.null chunk
        then return state
        else appendHelp handle =<< appendByteString state chunk
