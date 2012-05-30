
module Main where

import Control.Monad (msum,guard)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.List (isPrefixOf, isSuffixOf)
import Happstack.Server
import Happstack.Server.Compression
import System.Environment
import qualified Language.Elm as Elm

serve :: String -> IO ()
serve libLoc = do
  putStrLn "Elm Server 0.1.2: running at <http://localhost:8000>"
  simpleHTTP nullConf $ do
         _ <- compressedResponseFilter
         msum [ uriRest (serveElm libLoc)
              , serveDirectory EnableBrowsing [] "."
              ]

pageTitle :: String -> String
pageTitle fp =
    reverse . takeWhile (/='/') . drop 1 . dropWhile (/='.') $ reverse fp

serveElm libLoc fp = do
  let ('/':filePath) = fp
  guard (".elm" `isSuffixOf` filePath)
  content <- liftIO (readFile filePath)
  ok . toResponse $ Elm.toHtml libLoc (pageTitle filePath) content


main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ("--help":_) = putStrLn usage
parse ("--version":_) = putStrLn "The Elm Server 0.1.2"
parse [] = serve "/elm-mini.js"
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
