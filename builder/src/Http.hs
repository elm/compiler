{-# LANGUAGE OverloadedStrings #-}
module Http
  ( Manager
  , getManager
  , toUrl
  -- fetch
  , get
  , post
  , Header
  , accept
  , Error(..)
  -- archives
  , Sha
  , shaToChars
  , getArchive
  , writePackage
  -- upload
  , upload
  , filePart
  , jsonPart
  , stringPart
  )
  where


import Prelude hiding (zip)
import qualified Codec.Archive.Zip as Zip
import Control.Exception (SomeException, handle)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.String as String
import qualified Data.List as List
import Network.HTTP (urlEncodeVars)
import Network.HTTP.Client hiding (path)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (Header, hAccept, hAcceptEncoding, hUserAgent)
import Network.HTTP.Types.Method (Method, methodGet, methodPost)
import qualified Network.HTTP.Client as Multi (RequestBody(RequestBodyLBS))
import qualified Network.HTTP.Client.MultipartFormData as Multi
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import System.FilePath ((</>))

import qualified Json.Encode as Encode
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



-- FETCH


get :: Manager -> String -> [Header] -> (Error -> e) -> (BS.ByteString -> IO (Either e a)) -> IO (Either e a)
get =
  fetch methodGet


post :: Manager -> String -> [Header] -> (Error -> e) -> (BS.ByteString -> IO (Either e a)) -> IO (Either e a)
post =
  fetch methodPost


fetch :: Method -> Manager -> String -> [Header] -> (Error -> e) -> (BS.ByteString -> IO (Either e a)) -> IO (Either e a)
fetch methodVerb manager url headers onError onSuccess =
  handle (handleSomeException url onError) $
  handle (handleHttpException url onError) $
  do  req0 <- parseUrlThrow url
      let req1 =
            req0
              { method = methodVerb
              , requestHeaders = addDefaultHeaders headers
              }
      withResponse req1 manager $ \response ->
        do  chunks <- brConsume (responseBody response)
            onSuccess (BS.concat chunks)


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



-- SHA


type Sha = SHA.Digest SHA.SHA1State


shaToChars :: Sha -> String
shaToChars =
  SHA.showDigest



-- FETCH ARCHIVE


getArchive
  :: Manager
  -> String
  -> (Error -> e)
  -> e
  -> ((Sha, Zip.Archive) -> IO (Either e a))
  -> IO (Either e a)
getArchive manager url onError err onSuccess =
  handle (handleSomeException url onError) $
  handle (handleHttpException url onError) $
  do  req0 <- parseUrlThrow url
      let req1 =
            req0
              { method = methodGet
              , requestHeaders = addDefaultHeaders []
              }
      withResponse req1 manager $ \response ->
        do  result <- readArchive (responseBody response)
            case result of
              Nothing -> return (Left err)
              Just shaAndArchive -> onSuccess shaAndArchive


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



-- WRITE PACKAGE


writePackage :: FilePath -> Zip.Archive -> IO ()
writePackage destination archive =
  case Zip.zEntries archive of
    [] ->
      return ()

    entry:entries ->
      do  let root = length (Zip.eRelativePath entry)
          mapM_ (writeEntry destination root) entries


writeEntry :: FilePath -> Int -> Zip.Entry -> IO ()
writeEntry destination root entry =
  let
    path = drop root (Zip.eRelativePath entry)
  in
  if List.isPrefixOf "src/" path && last path == '/'
  then Dir.createDirectoryIfMissing True (destination </> path)
  else
    if List.isPrefixOf "src/" path && (FP.isExtensionOf "elm" path || List.isPrefixOf "src/Elm/Kernel/" path && FP.isExtensionOf "js" path)
    || path == "LICENSE"
    || path == "README.md"
    || path == "elm.json"
    then LBS.writeFile (destination </> path) (Zip.fromEntry entry)
    else return ()



-- UPLOAD


upload :: Manager -> String -> [Multi.Part] -> IO (Either Error ())
upload manager url parts =
  handle (handleSomeException url id) $
  handle (handleHttpException url id) $
  do  req0 <- parseUrlThrow url
      req1 <-
        Multi.formDataBody parts $
          req0
            { method = methodPost
            , requestHeaders = addDefaultHeaders []
            , responseTimeout = responseTimeoutNone
            }
      withResponse req1 manager $ \_ ->
        return (Right ())


filePart :: String -> FilePath -> Multi.Part
filePart name filePath =
  Multi.partFileSource (String.fromString name) filePath


jsonPart :: String -> FilePath -> Encode.Value -> Multi.Part
jsonPart name filePath value =
  let
    body =
      Multi.RequestBodyLBS $ B.toLazyByteString $ Encode.encodeUgly value
  in
  Multi.partFileRequestBody (String.fromString name) filePath body


stringPart :: String -> String -> Multi.Part
stringPart name string =
  Multi.partBS (String.fromString name) (BS.pack string)
