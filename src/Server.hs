{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (head,span,id)
import Control.Monad (msum,guard)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.List (isPrefixOf, isSuffixOf)
import Happstack.Server hiding (body)
import Happstack.Server.Compression
import System.Environment
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (title,span,dir,style)
import Text.Blaze.Internal
import ToHtml

serverConf = Conf { port = 80
                  , validator = validator nullConf
                  , tls = tls nullConf
                  , logAccess = logAccess nullConf
                  , timeout = timeout nullConf }

serve :: String -> IO ()
serve libLoc = do
  putStrLn "Elm Server 0.1: running at <http://localhost:8000>"
  simpleHTTP nullConf $ do
         compressedResponseFilter
         msum [ uriRest (serveElm libLoc)
              , serveDirectory EnableBrowsing [] "."
              ]

serveElm libLoc fp = do
  let ('/':path) = fp
  guard (".elm" `isSuffixOf` path)
  content <- liftIO (readFile path)
  ok . toResponse $ compileToHtml libLoc path content


main = getArgs >>= parse

parse ("--help":_) = putStrLn usage
parse ("--version":_) = putStrLn "The Elm Server 0.1"
parse [] = serve "/elm-mini.js"
parse [arg]
    | "--runtime-location=" `isPrefixOf` arg =
        serve . tail $ dropWhile (/='=') arg
    | otherwise = putStrLn usageMini
parse _ = putStrLn usageMini

usageMini =
  "Usage: elm-server [OPTIONS]\n\
  \Try `elm-server --help' for more information."

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
