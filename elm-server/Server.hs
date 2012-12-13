
module Main where

import Control.Monad (msum,guard)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.List (isPrefixOf, isSuffixOf)
import Data.Version (showVersion)
import Happstack.Server
import Happstack.Server.Compression
import System.Environment
import System.FilePath
import qualified Language.Elm as Elm
import Paths_elm_server

runtime = "/elm-" ++ showVersion version ++ ".js"

serve :: String -> IO ()
serve libLoc = do
  putStrLn ("Elm Server " ++ showVersion version ++ ": running at <http://localhost:8000>")
  putStrLn "Just refresh a page to recompile it!"
  simpleHTTP nullConf $ do
         _ <- compressedResponseFilter
         msum [ uriRest serveElm
              , uriRest (serveLib libLoc)
              , serveDirectory EnableBrowsing [] "."
              ]

pageTitle :: String -> String
pageTitle = dropExtension . takeBaseName

serveElm fp = do
  guard (takeExtension fp == ".elm")
  content <- liftIO (readFile (tail fp))
  ok . toResponse $ Elm.toHtml runtime (pageTitle fp) content

serveLib libLoc fp = do
  guard (fp == runtime)
  serveFile (asContentType "application/javascript") libLoc

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ("--help":_) = putStrLn usage
parse ("--version":_) = putStrLn ("The Elm Server " ++ showVersion version)
parse [] = serve =<< Elm.runtimeLocation
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
  \  --runtime-location   set the location of the Elm runtime\n\
  \\n\
  \Compiler Information:\n\
  \  --version            print the version information and exit\n\
  \  --help               display this help and exit\n\
  \\n\
  \Elm home page: <http://elm-lang.org>"
