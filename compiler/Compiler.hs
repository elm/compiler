{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Monad (foldM, when)
import Control.Applicative ((<$>), (<*>))
import qualified Data.Map as Map
import Data.Either (lefts, rights)
import Data.List (intersect, intercalate, lookup, partition)
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

import SourceSyntax.Module
import Initialize (buildFromSource, getSortedModuleNames)
import Generate.JavaScript (jsModule)
import Generate.Html (createHtml, JSStyle(..), JSSource(..))
import qualified Metadata.Libraries as Libraries
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

file :: Flags -> FilePath -> String -> FilePath
file flags filePath ext = output_directory flags </> replaceExtension filePath ext

elmo :: Flags -> FilePath -> FilePath
elmo flags filePath = file flags filePath "elmo"


buildFile :: Flags -> Map.Map String Interface -> Int -> Int -> FilePath -> IO Interface
buildFile flags otherInterfaces moduleNum numModules filePath =
    do compiled <- alreadyCompiled
       if compiled then readFile (elmo flags filePath) else compile

    where
      alreadyCompiled :: IO Bool
      alreadyCompiled = do
        exists <- doesFileExist (elmo flags filePath)
        if not exists then return False
                      else do tsrc <- getModificationTime filePath
                              tint <- getModificationTime (elmo flags filePath)
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
                  Right modul -> do exs <- exportInfo (modul :: Module () String)
                                    return (exs, jsModule modul)
        createDirectoryIfMissing True ((output_directory flags) </> takeDirectory filePath)
        writeFile (elmo flags filePath) obj
        return obj


getRuntime :: Flags -> IO FilePath
getRuntime flags =
    case runtime flags of
      Just fp -> return fp
      Nothing -> getDataFileName "elm-runtime.js"

build :: Flags -> FilePath -> IO ()
build flags rootFile = do
  allFiles <- if make flags then getSortedModuleNames rootFile else return [rootFile]
  let (nativeFiles, files) = partition (\fn -> takeExtension fn == ".js") allFiles
  buildFiles flags (length files) Map.empty files
  js <- flip (foldM appendJSToOutput) nativeFiles =<< foldM appendToOutput "" files
  case only_js flags of
    True -> do
      putStr "Generating JavaScript ... "
      writeFile (file flags rootFile "js") (genJs js)
      putStrLn "Done"
    False -> do
      putStr "Generating HTML ... "
      runtime <- getRuntime flags
      let html = genHtml $ createHtml runtime rootFile (sources js) ""
      writeFile (file flags rootFile "html") html
      putStrLn "Done"

    where
      appendToOutput :: String -> FilePath -> IO String
      appendToOutput js filePath =
          do src <- readFile (elmo flags filePath)
             return (src ++ js)

      appendJSToOutput :: String -> FilePath -> IO String
      appendJSToOutput js filePath =
          do src <- readFile filePath
             return (src ++ js)

      genHtml = if minify flags then Normal.renderHtml else Pretty.renderHtml
      genJs = if minify flags then BS.unpack . JS.minify . BS.pack else id
      sources js = map Link (scripts flags) ++
                   [ Source (if minify flags then Minified else Readable) js ]


buildFiles :: Flags -> Int -> Map.Map String Interface -> [FilePath] -> IO ()
buildFiles _ _ _ [] = return ()
buildFiles flags numModules interfaces (filePath:rest) = do
  interface <- buildFile flags interfaces (numModules - length rest) numModules filePath
  let moduleName = intercalate "." (splitDirectories (dropExtensions filePath))
      interfaces' = Map.insert moduleName interface interfaces
  buildFiles flags numModules interfaces' rest


exportInfo :: Module t v -> IO String
exportInfo (Module names exs ims stmts) =
    do print exs
       return (show exs)