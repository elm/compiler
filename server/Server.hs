
module Main where

import Control.Monad (msum,guard)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.List (isPrefixOf, isSuffixOf, (\\))
import Data.Version (showVersion)
import Happstack.Server
import Happstack.Server.Compression
import System.Environment
import System.FilePath
import qualified Language.Elm as Elm
import Paths_elm_server

runtime = "/elm-" ++ showVersion version ++ ".js"

serve :: Int -> String -> IO ()
serve portNumber libLoc = do
  putStrLn ("Elm Server " ++ showVersion version ++
            ": running at <http://localhost:" ++ (show portNumber) ++ ">")
  putStrLn "Just refresh a page to recompile it!"
  simpleHTTP httpConf $ do
         _ <- compressedResponseFilter
         msum [ uriRest serveElm
              , uriRest (serveLib libLoc)
              , serveDirectory EnableBrowsing [] "."
              ]
  where httpConf = nullConf { port = portNumber }

pageTitle :: String -> String
pageTitle = dropExtension . takeBaseName

serveElm fp = do
  guard (takeExtension fp == ".elm")
  content <- liftIO (readFile (tail fp))
  length content `seq` (ok . toResponse $ Elm.toHtml runtime (pageTitle fp) content)

serveLib libLoc fp = do
  guard (fp == runtime)
  serveFile (asContentType "application/javascript") libLoc

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ("--help":_) = putStrLn usage
parse ("--version":_) = putStrLn ("The Elm Server " ++ showVersion version)
parse args = if null remainingArgs
             then serve portNumber =<< elmRuntime
             else putStrLn usageMini
  where runtimeArg = filter (isPrefixOf "--runtime-location=") args
        portArg = filter (isPrefixOf "--port=") args
        remainingArgs = (args \\ runtimeArg) \\ portArg

        argValue arg = tail $ dropWhile (/= '=') (head arg)
        portNumber = if null portArg then 8000 else read (argValue portArg) :: Int
        elmRuntime = if null runtimeArg then Elm.runtime else return $ argValue runtimeArg

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
  \Server configuration:\n\
  \  --port               set the port to listen on (default: 8000)\n\
  \\n\
  \Resource Locations:\n\
  \  --runtime-location   set the location of the Elm runtime\n\
  \\n\
  \Compiler Information:\n\
  \  --version            print the version information and exit\n\
  \  --help               display this help and exit\n\
  \\n\
  \Elm home page: <http://elm-lang.org>"
