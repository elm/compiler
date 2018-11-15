{-# LANGUAGE OverloadedStrings #-}
module Develop
  ( Flags(..)
  , run
  )
  where


import Control.Applicative ((<|>))
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (guard, void)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid ((<>))
import qualified System.Directory as Dir
import System.FilePath as FP
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

import qualified Elm.Project as Project
import qualified Develop.Generate.Help as Generate
import qualified Develop.Generate.Index as Index
import qualified Develop.StaticFiles as StaticFiles
import qualified Generate.Output as Output
import qualified Reporting.Exit as Exit
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task



-- RUN THE DEV SERVER


data Flags =
  Flags
    { _port :: Maybe Int
    }


run :: () -> Flags -> IO ()
run () (Flags maybePort) =
  let
    port =
      maybe 8000 id maybePort
  in
    do  putStrLn $ "Go to <http://localhost:" ++ show port ++ "> to see your project dashboard."

        httpServe (config port) $
          serveFiles
          <|> serveDirectoryWith directoryConfig "."
          <|> serveAssets
          <|> error404


config :: Int -> Config Snap a
config port =
  defaultConfig
    # setVerbose False
    # setPort port
    # setAccessLog ConfigNoLog
    # setErrorLog ConfigNoLog


(#) :: a -> (a -> b) -> b
(#) value func =
  func value



-- INDEX


directoryConfig :: MonadSnap m => DirectoryConfig m
directoryConfig =
  let
    customGenerator pwd =
      do  modifyResponse $ setContentType "text/html; charset=utf-8"
          html <- liftIO $
            do  root <- Dir.getCurrentDirectory
                Index.get root pwd
          writeBuilder html
  in
    fancyDirectoryConfig
      { indexFiles = []
      , indexGenerator = customGenerator
      }



-- NOT FOUND


error404 :: Snap ()
error404 =
  do  modifyResponse $ setResponseStatus 404 "Not Found"
      modifyResponse $ setContentType "text/html; charset=utf-8"
      writeBuilder $ Generate.makePageHtml "NotFound" Nothing



-- SERVE FILES


serveFiles :: Snap ()
serveFiles =
  do  file <- getSafePath
      guard =<< liftIO (Dir.doesFileExist file)
      serveElm file <|> serveFilePretty file



-- SERVE FILES + CODE HIGHLIGHTING


serveFilePretty :: FilePath -> Snap ()
serveFilePretty file =
  let
    possibleExtensions =
      getSubExts (takeExtensions file)
  in
    case mconcat (map lookupMimeType possibleExtensions) of
      Nothing ->
        serveCode file

      Just mimeType ->
        serveFileAs mimeType file


getSubExts :: String -> [String]
getSubExts fullExtension =
  if null fullExtension then
    []

  else
    fullExtension : getSubExts (takeExtensions (drop 1 fullExtension))


serveCode :: String -> Snap ()
serveCode file =
  do  code <- liftIO (BS.readFile file)
      modifyResponse (setContentType "text/html")
      writeBuilder $
        Generate.makeCodeHtml ('~' : '/' : file) (B.byteString code)



-- SERVE ELM


serveElm :: FilePath -> Snap ()
serveElm file =
  do  guard (takeExtension file == ".elm")
      modifyResponse (setContentType "text/html")
      writeBuilder =<< liftIO (compileToHtmlBuilder file)


compileToHtmlBuilder :: FilePath -> IO B.Builder
compileToHtmlBuilder file =
  do  mvar1 <- newEmptyMVar
      mvar2 <- newEmptyMVar

      let reporter = Progress.Reporter (\_ -> return ()) (\_ -> return True) (putMVar mvar1)
      let output = Just (Output.HtmlBuilder mvar2)

      void $ Task.try reporter $
        do  summary <- Project.getRoot
            Project.compile Output.Dev Output.Client output Nothing summary [file]

      result <- takeMVar mvar1
      case result of
        Just exit ->
          return $ Generate.makePageHtml "Errors" (Just (Exit.toJson exit))

        Nothing ->
          takeMVar mvar2



-- SERVE STATIC ASSETS


serveAssets :: Snap ()
serveAssets =
  do  file <- getSafePath
      case StaticFiles.lookup file of
        Nothing ->
          pass

        Just (content, mimeType) ->
          do  modifyResponse (setContentType (mimeType <> ";charset=utf-8"))
              writeBS content



-- MIME TYPES


lookupMimeType :: FilePath -> Maybe BS.ByteString
lookupMimeType ext =
  HashMap.lookup ext mimeTypeDict


(==>) :: a -> b -> (a,b)
(==>) a b =
  (a, b)


mimeTypeDict :: HashMap.HashMap FilePath BS.ByteString
mimeTypeDict =
  HashMap.fromList
    [ ".asc"     ==> "text/plain"
    , ".asf"     ==> "video/x-ms-asf"
    , ".asx"     ==> "video/x-ms-asf"
    , ".avi"     ==> "video/x-msvideo"
    , ".bz2"     ==> "application/x-bzip"
    , ".css"     ==> "text/css"
    , ".dtd"     ==> "text/xml"
    , ".dvi"     ==> "application/x-dvi"
    , ".gif"     ==> "image/gif"
    , ".gz"      ==> "application/x-gzip"
    , ".htm"     ==> "text/html"
    , ".html"    ==> "text/html"
    , ".ico"     ==> "image/x-icon"
    , ".jpeg"    ==> "image/jpeg"
    , ".jpg"     ==> "image/jpeg"
    , ".js"      ==> "text/javascript"
    , ".json"    ==> "application/json"
    , ".m3u"     ==> "audio/x-mpegurl"
    , ".mov"     ==> "video/quicktime"
    , ".mp3"     ==> "audio/mpeg"
    , ".mpeg"    ==> "video/mpeg"
    , ".mpg"     ==> "video/mpeg"
    , ".ogg"     ==> "application/ogg"
    , ".pac"     ==> "application/x-ns-proxy-autoconfig"
    , ".pdf"     ==> "application/pdf"
    , ".png"     ==> "image/png"
    , ".qt"      ==> "video/quicktime"
    , ".sig"     ==> "application/pgp-signature"
    , ".spl"     ==> "application/futuresplash"
    , ".svg"     ==> "image/svg+xml"
    , ".swf"     ==> "application/x-shockwave-flash"
    , ".tar"     ==> "application/x-tar"
    , ".tar.bz2" ==> "application/x-bzip-compressed-tar"
    , ".tar.gz"  ==> "application/x-tgz"
    , ".tbz"     ==> "application/x-bzip-compressed-tar"
    , ".text"    ==> "text/plain"
    , ".tgz"     ==> "application/x-tgz"
    , ".ttf"     ==> "application/x-font-truetype"
    , ".txt"     ==> "text/plain"
    , ".wav"     ==> "audio/x-wav"
    , ".wax"     ==> "audio/x-ms-wax"
    , ".wma"     ==> "audio/x-ms-wma"
    , ".wmv"     ==> "video/x-ms-wmv"
    , ".xbm"     ==> "image/x-xbitmap"
    , ".xml"     ==> "text/xml"
    , ".xpm"     ==> "image/x-xpixmap"
    , ".xwd"     ==> "image/x-xwindowdump"
    , ".zip"     ==> "application/zip"
    ]

