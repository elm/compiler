{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Monad (foldM, when)
import qualified Data.Map as Map
import Data.Either (lefts, rights)
import Data.List (intersect, intercalate, lookup)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import System.Console.CmdArgs
import System.Directory
import System.Exit
import System.FilePath
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty
import qualified Text.Blaze.Html.Renderer.String as Normal

import qualified Text.Jasmine as JS
import qualified Data.ByteString.Lazy.Char8 as BS

import Ast
import Initialize (buildFromSource, getSortedModuleNames)
import CompileToJS (jsModule)
import GenerateHtml (createHtml, JSStyle(..), JSSource(..))
import qualified Libraries as Libraries
import Paths_Elm

data Flags =
    Flags { make :: Bool
          , files :: [FilePath]
          , runtime :: Maybe FilePath
          , only_js :: Bool
          , scripts :: [FilePath]
          , no_prelude :: Bool
          , minify :: Bool
	  , output_directory :: FilePath
          }
    deriving (Data,Typeable,Show,Eq)

flags = Flags
  { make = False
           &= help "automatically compile dependencies."
  , files = def &= args &= typ "FILES"
  , runtime = Nothing &= typFile
              &= help "Specify a custom location for Elm's runtime system."
  , only_js = False
              &= help "Compile only to JavaScript."
  , scripts = [] &= typFile
              &= help "Load JavaScript files in generated HTML. Files will be included in the given order."
  , no_prelude = False
                 &= help "Do not import Prelude by default, used only when compiling standard libraries."
  , minify = False
             &= help "Minify generated JavaScript and HTML"
  , output_directory = "ElmFiles" &= typFile
                       &= help "Output files to directory specified. Defaults to ElmFiles/ directory."
  } &= help "Compile Elm programs to HTML, CSS, and JavaScript."
    &= summary ("The Elm Compiler " ++ showVersion version ++ ", (c) Evan Czaplicki")

main :: IO ()             
main = compileArgs =<< cmdArgs flags

compileArgs :: Flags -> IO ()
compileArgs flags =
    case files flags of
      [] -> putStrLn "Usage: elm [OPTIONS] [FILES]\nFor more help: elm --help"
      fs -> mapM_ (build flags) fs
          

type Interface = String

buildFile :: Flags -> Int -> Int -> FilePath -> IO Interface
buildFile flags moduleNum numModules filePath =
    do compiled <- alreadyCompiled
       if compiled then readFile elmo else compile

    where
      file :: String -> FilePath
      file ext = output_directory flags </> replaceExtension filePath ext

      elmo :: FilePath
      elmo = file "elmo"

      alreadyCompiled :: IO Bool
      alreadyCompiled = do
        exists <- doesFileExist elmo
        if not exists then return False
                      else do tsrc <- getModificationTime filePath
                              tint <- getModificationTime elmo
                              return (tsrc < tint)

      number :: String
      number = "[" ++ show moduleNum ++ " of " ++ show numModules ++ "]"

      name :: String
      name = intercalate "." (splitDirectories (dropExtensions filePath))

      compile :: IO Interface
      compile = do
        putStrLn (number ++ " Compiling " ++ name)
        source <- readFile filePath
        (interface,obj) <-
            if takeExtension filePath == ".js" then return ("",source) else
                case buildFromSource (no_prelude flags) source of
                  Left err -> putStrLn err >> exitFailure
                  Right modul -> do exs <- exportInfo modul
                                    return (exs, jsModule modul)
        createDirectoryIfMissing True (output_directory flags)
        writeFile elmo obj
        return obj


getRuntime :: Flags -> IO FilePath
getRuntime flags =
    case runtime flags of
      Just fp -> return fp
      Nothing -> getDataFileName "elm-runtime.js"

build :: Flags -> FilePath -> IO ()
build flags rootFile = do
  files <- if make flags then getSortedModuleNames rootFile else return [rootFile]
  buildFiles flags (length files) Map.empty files
  js <- foldM appendToOutput "" files
  case only_js flags of
    True -> do
      putStr "Generating JavaScript ... "
      writeFile (replaceExtension rootFile "js") (genJs js)
      putStrLn "Done"
    False -> do
      putStr "Generating HTML ... "
      runtime <- getRuntime flags
      let html = genHtml $ createHtml runtime rootFile (sources js) ""
      writeFile (replaceExtension rootFile "html") html
      putStrLn "Done"

    where
      appendToOutput :: String -> FilePath -> IO String
      appendToOutput js filePath =
          do src <- readFile (output_directory flags </> replaceExtension filePath "elmo")
             return (src ++ js)

      genHtml = if minify flags then Normal.renderHtml else Pretty.renderHtml
      genJs = if minify flags then BS.unpack . JS.minify . BS.pack else id
      sources js = map Link (scripts flags) ++
                   [ Source (if minify flags then Minified else Readable) js ]


buildFiles :: Flags -> Int -> Map.Map String Interface -> [FilePath] -> IO ()
buildFiles _ _ _ [] = return ()
buildFiles flags numModules interfaces (filePath:rest) = do
  interface <- buildFile flags (numModules - length rest) numModules filePath
  let moduleName = intercalate "." (splitDirectories (dropExtensions filePath))
      interfaces' = Map.insert moduleName interface interfaces
  buildFiles flags numModules interfaces' rest


exportInfo :: Module -> IO String
exportInfo (Module names ims exs stmts) =
    do print exs
       return (show exs)