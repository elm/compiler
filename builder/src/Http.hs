{-# LANGUAGE OverloadedStrings #-}
module Http
  ( Manager
  , getManager
  , toUrl
  , post
  , post'
  , Header
  , accept
  , Timeout(..)
  , Body(..)
  , Multi.Part
  , filePart
  , stringPart
  , BodyReader
  , readByteString
  , Sha
  , shaToChars
  , readArchive
  , writeArchive
  , Error(..)
  )
  where


import Prelude hiding (zip)
import qualified Codec.Archive.Zip as Zip
import Control.Exception (SomeException, handle)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Char8 as BS
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.String as String
import Network.HTTP (urlEncodeVars)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (Header, hAccept, hAcceptEncoding, hUserAgent)
import Network.HTTP.Types.Method (methodPost)
import qualified Network.HTTP.Client.MultipartFormData as Multi
import qualified System.Directory as Dir

import qualified Elm.Version as V



-- MANAGER


getManager :: IO Manager
getManager =
  newManager tlsManagerSettings



-- URL


toUrl :: String -> [(String,String)] -> String
toUrl url params =
  case params of
    []  -> url
    _:_ -> url ++ "?" ++ urlEncodeVars params



-- POST


post
  :: Manager
  -> String
  -> [Header]
  -> (Error -> e)
  -> (BS.ByteString -> IO (Either e a))
  -> IO (Either e a)
post manager url headers onError onSuccess =
  handle (handleSomeException url onError) $
  handle (handleHttpException url onError) $
  do  req0 <- parseUrlThrow url
      let req1 =
            req0
              { method = methodPost
              , requestHeaders = addDefaultHeaders headers
              }
      withResponse req1 manager $ \response ->
        do  chunks <- brConsume (responseBody response)
            onSuccess (BS.concat chunks)


post'
  :: Manager
  -> String
  -> [Header]
  -> Timeout
  -> Body
  -> (Error -> e)
  -> (BodyReader -> IO (Either e a))
  -> IO (Either e a)
post' manager url headers timeout body onError onSuccess =
  handle (handleSomeException url onError) $
  handle (handleHttpException url onError) $
  do  req0 <- parseUrlThrow url
      req1 <-
        addBody body $
          addTimeout timeout $
            req0
              { method = methodPost
              , requestHeaders = addDefaultHeaders headers
              }
      withResponse req1 manager (onSuccess . responseBody)



-- HEADERS


addDefaultHeaders :: [Header] -> [Header]
addDefaultHeaders headers =
  (hUserAgent, userAgent) : (hAcceptEncoding, "gzip") : headers


{-# NOINLINE userAgent #-}
userAgent :: BS.ByteString
userAgent =
  BS.pack ("elm/" ++ V.toChars V.compiler)


accept :: BS.ByteString -> Header
accept mime =
  (hAccept, mime)



-- TIMEOUT


data Timeout
  = DefaultTimeout
  | NeverTimeout


addTimeout :: Timeout -> Request -> Request
addTimeout timeout req =
  case timeout of
    DefaultTimeout ->
      req

    NeverTimeout ->
      req { responseTimeout = responseTimeoutNone }



-- BODY


data Body
  = NoBody
  | FormDataBody [Multi.Part]


addBody :: Body -> Request -> IO Request
addBody body req =
  case body of
    NoBody ->
      return req

    FormDataBody parts ->
      Multi.formDataBody parts req


filePart :: String -> FilePath -> Multi.Part
filePart name filePath =
  Multi.partFileSource (String.fromString name) filePath


stringPart :: String -> String -> Multi.Part
stringPart name string =
  Multi.partBS  (String.fromString name) (BS.pack string)



-- EXCEPTIONS


data Error
  = BadUrl String String
  | BadHttp String HttpExceptionContent
  | BadMystery String SomeException


handleHttpException :: String -> (Error -> e) -> HttpException -> IO (Either e a)
handleHttpException url onError httpException =
  case httpException of
    InvalidUrlException _ reason ->
      return (Left (onError (BadUrl url reason)))

    HttpExceptionRequest _ content ->
      return (Left (onError (BadHttp url content)))


handleSomeException :: String -> (Error -> e) -> SomeException -> IO (Either e a)
handleSomeException url onError exception =
  return (Left (onError (BadMystery url exception)))



-- BODY READERS


readByteString :: BodyReader -> IO BS.ByteString
readByteString bodyReader =
  BS.concat <$> brConsume bodyReader


type Sha = SHA.Digest SHA.SHA1State


shaToChars :: Sha -> String
shaToChars =
  SHA.showDigest


readArchive :: BodyReader -> IO (Maybe (Sha, Zip.Archive))
readArchive body =
  readArchiveHelp body $
    AS 0 SHA.sha1Incremental (Binary.runGetIncremental Binary.get)


data ArchiveState =
  AS
    { _len :: !Int
    , _sha :: !(Binary.Decoder SHA.SHA1State)
    , _zip :: !(Binary.Decoder Zip.Archive)
    }


readArchiveHelp :: BodyReader -> ArchiveState -> IO (Maybe (Sha, Zip.Archive))
readArchiveHelp body (AS len sha zip) =
  case zip of
    Binary.Fail _ _ _ ->
      return Nothing

    Binary.Partial k ->
      do  chunk <- brRead body
          readArchiveHelp body $
            AS
              { _len = len + BS.length chunk
              , _sha = Binary.pushChunk sha chunk
              , _zip = k (if BS.null chunk then Nothing else Just chunk)
              }

    Binary.Done _ _ archive ->
      return $ Just ( SHA.completeSha1Incremental sha len, archive )



-- WRITE ARCHIVE


writeArchive :: Zip.Archive -> FilePath -> FilePath -> IO ()
writeArchive archive destination newRoot =
  do  Dir.createDirectoryIfMissing True destination
      let opts = [Zip.OptDestination destination]
      mapM_ (Zip.writeEntry opts . replaceRoot newRoot) (Zip.zEntries archive)


replaceRoot :: String -> Zip.Entry -> Zip.Entry
replaceRoot root entry =
  let
    rootless =
      dropWhile (/='/') (Zip.eRelativePath entry)
  in
    entry { Zip.eRelativePath = root ++ rootless }
