{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Data.Either (lefts, rights)
import Data.List (intersect, intercalate)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import System.Console.CmdArgs
import System.Exit
import System.FilePath
import Text.Blaze.Html.Renderer.String (renderHtml)

import qualified Text.Jasmine as JS
import qualified Data.ByteString.Lazy.Char8 as BS

import Ast
import Initialize
import CompileToJS
import GenerateHtml
import Paths_Elm

data ELM =
    ELM { make :: Bool
        , files :: [FilePath]
        , runtime :: Maybe FilePath
        , separate_js :: Bool
        , only_js :: Bool
        , import_js :: [FilePath]
        , no_prelude :: Bool
        , noscript :: Bool
        , minify :: Bool
	, output_directory :: Maybe FilePath
        }
    deriving (Data,Typeable,Show,Eq)

elm = ELM { make = False &= help "automatically compile dependencies."
          , files = def &= args &= typ "FILES"
          , runtime = Nothing &= typFile &=
            help "Specify a custom location for Elm's runtime system."
          , separate_js = False &= help "Compile to separate HTML and JS files."
          , only_js = False &= help "Compile only to JavaScript."
          , import_js = [] &= typFile &= help "Include a JavaScript file before the body of the Elm program. Can be used many times. Files will be included in the given order."
          , no_prelude = False &= help "Do not import Prelude by default, used only when compiling standard libraries."
          , noscript = True &= help "Add generated <noscript> tag to HTML output."
          , minify = False &= help "Minify generated JavaScript"
	  , output_directory = Nothing &= typFile &= help "Output files to directory specified. Defaults to the location of original file."
          } &=
    help "Compile Elm programs to HTML, CSS, and JavaScript." &=
    summary ("The Elm Compiler " ++ showVersion version ++ ", (c) Evan Czaplicki")

main = do
  args <- cmdArgs elm
  mini <- getDataFileName "elm-runtime.js"
  compileArgs mini args

compileArgs mini flags =
  case files flags of
    [] -> putStrLn "Usage: elm [OPTIONS] [FILES]\nFor more help: elm --help"
    fs -> mapM_ (fileTo flags what loc) fs
      where loc  = fromMaybe mini (runtime flags)
            what | only_js flags = JS
                 | separate_js flags = Split
                 | otherwise = HTML

data What = JS | HTML | Split

fileTo flags what rtLoc file = do
  let jsStyle  = if minify flags then Minified else Readable
      formatJS = if minify flags then BS.unpack . JS.minify . BS.pack else id
  ems <- if make flags then build file
                       else do src <- readFile file
                               return (fmap (:[]) (buildFromSource src))
  jss <- concat `fmap` mapM readFile (import_js flags)
  case ems of
    Left err -> do putStrLn $ "Error while compiling " ++ file ++ ":\n" ++ err
                   exitFailure
    Right ms' ->
        let path = fromMaybe "" (output_directory flags) </> file
            js = replaceExtension path ".js"
            html = replaceExtension path ".html"
            addPrelude (Module name exs ims stmts) =
                Module name exs (("Prelude", Importing []) : ims) stmts
            ms = if no_prelude flags then ms' else map addPrelude ms'
            txt = jss ++ concatMap jsModule ms
        in  case what of
              JS    -> writeFile js (formatJS txt)
              HTML  -> writeFile html . renderHtml $
                       modulesToHtml jsStyle "" rtLoc jss (noscript flags) ms
              Split ->
                  do writeFile html . renderHtml $ linkedHtml rtLoc js ms
                     writeFile js (formatJS txt)
