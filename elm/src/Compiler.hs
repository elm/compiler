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
        , generate_noscript :: Bool
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
          , generate_noscript = True &= help "Add generated <noscript> tag to HTML output."
          , minify = False &= help "Minify generated JavaScript"
	  , output_directory = Nothing &= typFile &= help "Output files to directory specified. Defaults to the location of original file."
          } &=
    help "Compile Elm programs to HTML, CSS, and JavaScript." &=
    summary ("The Elm Compiler " ++ showVersion version ++ ", (c) Evan Czaplicki")

main = do
  args <- cmdArgs elm
  mini <- getDataFileName ("elm-runtime-" ++ showVersion version ++ ".js")
  compileArgs mini args

compileArgs mini (ELM _ [] _ _ _ _ _ _ _) =
    putStrLn "Usage: elm [OPTIONS] [FILES]\nFor more help: elm --help"
compileArgs mini (ELM make files rtLoc split only js nscrpt isMini outputDir) =
    mapM_ (fileTo isMini make what js nscrpt outputDir loc) files
        where loc = fromMaybe mini rtLoc
              what | only = JS
                   | split = Split
                   | otherwise = HTML

data What = JS | HTML | Split

fileTo isMini make what jsFiles noscript outputDir rtLoc file = do
  let jsStyle  = if isMini then Minified else Readable
      formatJS = if isMini then BS.unpack . JS.minify . BS.pack else id
  ems <- build make file
  jss <- concat `fmap` mapM readFile jsFiles
  case ems of
    Left err -> do putStrLn $ "Error while compiling " ++ file ++ ":\n" ++ err
                   exitFailure
    Right ms ->
        let path = fromMaybe "" outputDir </> file
            js = replaceExtension path ".js"
            html = replaceExtension path ".html"
            pairs = map ((,) (fst ms)) (snd ms)
            txt = jss ++ concatMap jsModule pairs
        in  case what of
              JS    -> writeFile js (formatJS txt)
              HTML  -> writeFile html . renderHtml $
                       modulesToHtml jsStyle "" rtLoc jss noscript pairs
              Split ->
                  do writeFile html . renderHtml $ linkedHtml rtLoc js pairs
                     writeFile js (formatJS txt)
