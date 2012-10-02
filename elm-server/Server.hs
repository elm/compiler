
module Main where

import Control.Monad (msum,guard)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.List (isPrefixOf, isSuffixOf)
import Happstack.Server
import Happstack.Server.Compression
import System.Directory (makeRelativeToCurrentDirectory)
import System.Environment
import qualified Language.Elm as Elm
import Paths_Elm

serve :: String -> IO ()
serve libLoc = do
  putStrLn "Elm Server 0.4.0.3: running at <http://localhost:8000>"
  simpleHTTP nullConf $ do
         _ <- compressedResponseFilter
         msum [ uriRest serveElm
              , uriRest (serveLib libLoc)
              , serveDirectory EnableBrowsing [] "."
              ]

pageTitle :: String -> String
pageTitle fp =
    reverse . takeWhile (/='/') . drop 1 . dropWhile (/='.') $ reverse fp

serveElm fp = do
  let ('/':filePath) = fp
  guard (".elm" `isSuffixOf` filePath)
  content <- liftIO (readFile filePath)
  ok . toResponse $ Elm.toHtml "elm-runtime.js" (pageTitle filePath) content

serveLib libLoc fp = do
  guard (fp == "/elm-runtime.js")
  serveFile (asContentType "application/javascript") libLoc

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ("--help":_) = putStrLn usage
parse ("--version":_) = putStrLn "The Elm Server 0.4.0.3"
parse [] = do
  absolutePath <- getDataFileName "elm-runtime-0.4.0.3.js"
  runtime <- makeRelativeToCurrentDirectory absolutePath
  serve runtime
parse [arg]
    | "--runtime-location=" `isPrefixOf` arg =
        serve . tail $ dropWhile (/='=') arg
    | otherwise = putStrLn usageMini
parse _ = putStrLn usageMini

usageMini :: String
usageMini =
  "Usage: elm-server [OPTIONS]\n\
  \Try `elm-server --help' for more information."

usage :: String
usage =
  "Usage: elm-server [OPTIONS]\n\
  \Compiles and serves .elm files from the current directory.\n\
  \Example: elm-server\n\
  \\n\
  \Resource Locations:\n\
  \  --runtime-location   set the location of the Elm runtime (elm-mini.js)\n\
  \\n\
  \Compiler Information:\n\
  \  --version            print the version information and exit\n\
  \  --help               display this help and exit\n\
  \\n\
  \Elm home page: <http://elm-lang.org>"
