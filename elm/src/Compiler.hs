{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Data.Either (lefts, rights)
import Data.List (intersect, intercalate)
import Data.Maybe (fromMaybe)
import System.Console.CmdArgs
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
          } &=
    help "Compile Elm programs to HTML, CSS, and JavaScript." &=
    summary "The Elm Compiler v0.4.0.3, (c) Evan Czaplicki"

main = do
  args <- cmdArgs elm
  mini <- getDataFileName "elm-runtime-0.4.0.3.js"
  compileArgs mini args

compileArgs mini (ELM _ [] _ _ _ _ _ _) =
    putStrLn "Usage: elm [OPTIONS] [FILES]\nFor more help: elm --help"
compileArgs mini (ELM make files rtLoc split only js nscrpt isMini) =
    mapM_ (fileTo isMini get what js nscrpt $ fromMaybe mini rtLoc) files
        where get = if make then getModules [] else getModule
              what = if only then JS else
                         if split then Split else HTML

data What = JS | HTML | Split

fileTo isMini get what jsFiles noscript rtLoc file = do
  let jsStyle = if isMini then Minified else Readable
  let formatJS = if isMini then BS.unpack . JS.minify . BS.pack else id
  ems <- get file
  jss <- concat `fmap` mapM readFile jsFiles
  case ems of
    Left err -> putStrLn $ "Error while compiling " ++ file ++ ":\n" ++ err
    Right ms ->
        let name = reverse . tail . dropWhile (/='.') $ reverse file
            js = name ++ ".js"
            html = name ++ ".html"
        in  case what of
              JS -> writeFile js . formatJS $ jss ++ concatMap jsModule ms
              HTML -> writeFile html . renderHtml $ modulesToHtml jsStyle "" rtLoc jss noscript ms
              Split -> do
                  writeFile html . renderHtml $ linkedHtml rtLoc js ms
                  writeFile js . formatJS $ jss ++ concatMap jsModule ms

getModules :: [String] -> FilePath -> IO (Either String [([String],Module)])
getModules uses file = do
  code <- readFile file
  case initialize code of
    Left err -> return . Left $ "Error in " ++ file ++ ":\n" ++ err
    Right (escs, modul@(Module _ _ imports _)) ->
        let imps = filter (`notElem` builtInModules) $ map fst imports in
        case intersect uses imps of
          x:_ -> return . Left $ "Error: Cyclic dependency. Module " ++
                                 x ++ " depends on itself."
          [] -> do
            ems <- mapM (getModules (uses ++ imps) . toFilePath) imps
            return $ case lefts ems of
              [] -> Right $ concat (rights ems) ++ [(escs,modul)]
              errs -> Left $ intercalate "\n" errs

getModule :: FilePath -> IO (Either String [([String],Module)])
getModule file = do
  code <- readFile file
  return . fmap (:[]) $ initialize code

toFilePath :: String -> FilePath
toFilePath modul = map (\c -> if c == '.' then '/' else c) modul ++ ".elm"

builtInModules =
    concat [ map ("Data."++)     [ "List", "Char", "Maybe" ]
           , map ("Signal."++)   [ "Mouse", "Keyboard.Raw"
                                 , "Window", "Time", "HTTP", "Input", "Random" ]
           , map ("Graphics."++) [ "Element", "Text", "Color" ]
           , map ("Foreign.JavaScript"++) [ "", "JSON", "Experimental" ]
           , [ "Prelude" ]
           ]