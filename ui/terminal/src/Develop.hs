{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Develop (run) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Monad (guard)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Char8 as BSC
import System.Directory
import System.FilePath
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze

import qualified CommandLine.Args as Args
import qualified Develop.Compile as Compile
import qualified Develop.Generate.Help as Generate
import qualified Develop.Generate.Index as Index
import qualified Develop.Generate.NotFound as NotFound
import qualified Develop.StaticFiles as StaticFiles



-- RUN THE DEV SERVER


run :: Args.DevFlags -> IO ()
run (Args.DevFlags maybePort) =
  -- TODO get `elm reactor` running again
  if True then
    putStrLn $
      "The reactor is not available in the ALPHA period.\n\
      \\n\
      \The goal is for package authors to get code updated and give early feedback.\n\
      \Professionals and hobbyists should NOT be ugrading at this time!\n"
  else
  let
    port =
      maybe 8000 id maybePort
  in
    do  putStrLn (startupMessage port)

        httpServe (config port) $
          serveFiles
          <|> route [ ("_compile", compile) ]
          <|> route [ ("_elm/move-to-root", moveToRoot)]
          <|> route [ ("_elm/create-new-project", createNewProject)]
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



-- HELPERS


startupMessage :: Int -> String
startupMessage port =
  "Go to <http://localhost:" ++ show port ++ "> to see your project dashboard."


directoryConfig :: MonadSnap m => DirectoryConfig m
directoryConfig =
  let
    customGenerator directory =
      do  project <- liftIO (Index.getProject directory)
          modifyResponse $ setContentType "text/html; charset=utf-8"
          writeBuilder (Blaze.renderHtmlBuilder (Index.toHtml project))
  in
    fancyDirectoryConfig
      { indexFiles = []
      , indexGenerator = customGenerator
      }


compile :: Snap ()
compile =
  do  file <- getSafePath
      guard =<< liftIO (doesFileExist file)
      modifyResponse (setContentType "text/javascript")
      writeBS =<< liftIO (Compile.toJavaScript file)


error404 :: Snap ()
error404 =
  do  modifyResponse $ setResponseStatus 404 "Not Found"
      modifyResponse $ setContentType "text/html; charset=utf-8"
      writeBuilder (Blaze.renderHtmlBuilder NotFound.html)



-- CREATE NEW PROJECT


createNewProject :: Snap ()
createNewProject =
  do  liftIO $ threadDelay (2 * 1000 * 1000)
      return ()


moveToRoot :: Snap ()
moveToRoot =
  liftIO Index.moveToRoot



-- SERVE FILES


serveFiles :: Snap ()
serveFiles =
  do  file <- getSafePath
      guard =<< liftIO (doesFileExist file)
      serveElm file <|> serveFilePretty file


serveHtml :: MonadSnap m => H.Html -> m ()
serveHtml html =
  do  modifyResponse (setContentType "text/html")
      writeBuilder (Blaze.renderHtmlBuilder html)



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
  do  code <- liftIO (readFile file)
      serveHtml $ Generate.makeCodeHtml ('~' : '/' : file) code



-- SERVE ELM


serveElm :: FilePath -> Snap ()
serveElm file =
  do  guard (takeExtension file == ".elm")
      serveHtml (Generate.makeElmHtml file)



-- SERVE STATIC ASSETS


serveAssets :: Snap ()
serveAssets =
  do  file <- getSafePath
      case StaticFiles.lookup file of
        Nothing ->
          pass

        Just (content, mimeType) ->
          do  modifyResponse (setContentType $ BSC.pack (mimeType ++ ";charset=utf-8"))
              writeBS content



-- MIME TYPES


lookupMimeType :: FilePath -> Maybe BSC.ByteString
lookupMimeType ext =
  HashMap.lookup ext mimeTypeDict


(==>) :: a -> b -> (a,b)
(==>) a b =
  (a, b)


mimeTypeDict :: HashMap.HashMap FilePath BSC.ByteString
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

