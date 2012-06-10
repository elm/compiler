module Main where

import Data.List (isPrefixOf)
import CompileToJS
import GenerateHtml
import System.Environment
import Text.Blaze.Html.Renderer.String (renderHtml)

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ("--help":_) = putStrLn usage
parse ("--version":_) = putStrLn "The Elm Compiler 0.2"
parse [loc,file]
  | "--runtime-location=" `isPrefixOf` loc =
      produceHtml (tail $ dropWhile (/='=') loc) file
  | otherwise = putStrLn usageMini
parse [file] = produceHtml "elm-mini.js" file
parse _ = putStrLn usageMini

produceHtml :: String -> FilePath -> IO ()
produceHtml libLoc file = do
  code <- readFile file
  let name = takeWhile (/='.') file
  case compile code of
    Left err -> putStrLn err
    Right jsCode -> writeFile (name ++ ".html") . renderHtml $
                    generateHtml libLoc name code

usageMini :: String
usageMini =
  "Usage: elm [OPTIONS] FILE\n\
  \Try `elm --help' for more information."

usage :: String
usage =
  "Usage: elm [OPTIONS] FILE\n\
  \Compile .elm files to .html files.\n\
  \Example: elm --runtime-location=../elm-mini.js main.elm\n\
  \\n\
  \Resource Locations:\n\
  \  --runtime-location   set the location of the Elm runtime (elm-mini.js)\n\
  \\n\
  \Compiler Information:\n\
  \  --version            print the version information and exit\n\
  \  --help               display this help and exit\n\
  \\n\
  \Elm home page: <http://elm-lang.org>"