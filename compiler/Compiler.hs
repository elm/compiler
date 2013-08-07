{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Binary as Binary
import Data.Version (showVersion)
import System.Console.CmdArgs hiding (program)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import GHC.Conc

import qualified Text.Blaze.Html.Renderer.Pretty as Pretty
import qualified Text.Blaze.Html.Renderer.String as Normal
import qualified Text.Jasmine as JS
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy as L

import qualified Metadata.Prelude as Prelude
import qualified Transform.Canonicalize as Canonical
import SourceSyntax.Module
import Parse.Module (getModuleName)
import Initialize (buildFromSource, getSortedDependencies)
import Generate.JavaScript (jsModule)
import Generate.Html (createHtml, JSStyle(..), JSSource(..))
import Paths_Elm

import SourceSyntax.PrettyPrint (pretty, variable)
import Text.PrettyPrint as P
import qualified Type.Type as Type
import qualified Data.Traversable as Traverse

data Flags =
    Flags { make :: Bool
          , files :: [FilePath]
          , runtime :: Maybe FilePath
          , only_js :: Bool
          , print_types :: Bool
          , print_program :: Bool
          , scripts :: [FilePath]
          , no_prelude :: Bool
          , minify :: Bool
	  , cache_dir :: FilePath
	  , build_dir :: FilePath
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
  , print_types = False
                  &= help "Print out infered types of top-level definitions."
  , print_program = False
                    &= help "Print out an internal representation of a program."
  , scripts = [] &= typFile
              &= help "Load JavaScript files in generated HTML. Files will be included in the given order."
  , no_prelude = False
                 &= help "Do not import Prelude by default, used only when compiling standard libraries."
  , minify = False
             &= help "Minify generated JavaScript and HTML"
  , cache_dir = "cache" &= typFile
                &= help "Directory for files cached to make builds faster. Defaults to cache/ directory."
  , build_dir = "build" &= typFile
                &= help "Directory for generated HTML and JS files. Defaults to build/ directory."
  } &= help "Compile Elm programs to HTML, CSS, and JavaScript."
    &= summary ("The Elm Compiler " ++ showVersion version ++ ", (c) Evan Czaplicki")

main :: IO ()             
main = do setNumCapabilities =<< getNumProcessors
          compileArgs =<< cmdArgs flags

compileArgs :: Flags -> IO ()
compileArgs flags =
    case files flags of
      [] -> putStrLn "Usage: elm [OPTIONS] [FILES]\nFor more help: elm --help"
      fs -> mapM_ (build flags) fs
          

buildPath :: Flags -> FilePath -> String -> FilePath
buildPath flags filePath ext = build_dir flags </> replaceExtension filePath ext

cachePath :: Flags -> FilePath -> String -> FilePath
cachePath flags filePath ext = cache_dir flags </> replaceExtension filePath ext

elmo :: Flags -> FilePath -> FilePath
elmo flags filePath = cachePath flags filePath "elmo"

elmi :: Flags -> FilePath -> FilePath
elmi flags filePath = cachePath flags filePath "elmi"


buildFile :: Flags -> Int -> Int -> Interfaces -> FilePath -> IO (String,ModuleInterface)
buildFile flags moduleNum numModules interfaces filePath =
    do compiled <- alreadyCompiled
       case compiled of
         False -> compile
         True -> do
           handle <- openBinaryFile (elmi flags filePath) ReadMode
           bits <- L.hGetContents handle
           let info :: (String, ModuleInterface)
               info = Binary.decode bits
           L.length bits `seq` hClose handle
           return info
    where
      alreadyCompiled :: IO Bool
      alreadyCompiled = do
        existsi <- doesFileExist (elmi flags filePath)
        existso <- doesFileExist (elmo flags filePath)
        if not existsi || not existso
            then return False
            else do tsrc <- getModificationTime filePath
                    tint <- getModificationTime (elmo flags filePath)
                    return (tsrc < tint)

      number :: String
      number = "[" ++ show moduleNum ++ " of " ++ show numModules ++ "]"

      compile :: IO (String,ModuleInterface)
      compile = do
        source <- readFile filePath
        let name = case getModuleName source of
                     Just n -> n
                     Nothing -> "Name"
        putStrLn $ concat [ number, " Compiling ", name
                          , replicate (max 1 (20 - length name)) ' '
                          , "( " ++ filePath ++ " )" ]
        
        createDirectoryIfMissing True (cache_dir flags)
        createDirectoryIfMissing True (build_dir flags)
        metaModule <-
            case buildFromSource (no_prelude flags) interfaces source of
              Left errors -> do
                  mapM print (List.intersperse (P.text " ") errors)
                  exitFailure
              Right modul -> do
                  case print_program flags of
                    False -> return ()
                    True -> print . pretty $ program modul
                  return modul
        
        if print_types flags then printTypes metaModule else return ()
        tipes <- Traverse.traverse Type.toSrcType (types metaModule)
        let interface = Canonical.interface name $ ModuleInterface {
                          iTypes = tipes,
                          iAdts = datatypes metaModule,
                          iAliases = aliases metaModule
                        }
        createDirectoryIfMissing True . dropFileName $ elmi flags filePath
        handle <- openBinaryFile (elmi flags filePath) WriteMode
        L.hPut handle (Binary.encode (name,interface))
        hClose handle
        writeFile (elmo flags filePath) (jsModule metaModule)
        return (name,interface)

printTypes metaModule = do
  putStrLn ""
  forM_ (Map.toList $ types metaModule) $ \(n,t) -> do
      pt <- Type.extraPretty t
      print $ variable n <+> P.text ":" <+> pt
  putStrLn ""

getRuntime :: Flags -> IO FilePath
getRuntime flags =
    case runtime flags of
      Just fp -> return fp
      Nothing -> getDataFileName "elm-runtime.js"

build :: Flags -> FilePath -> IO ()
build flags rootFile = do
  let noPrelude = no_prelude flags
  files <- if make flags then getSortedDependencies noPrelude rootFile else return [rootFile]
  let ifaces = if noPrelude then Map.empty else Prelude.interfaces
  (moduleName, interfaces) <- buildFiles flags (length files) ifaces "" files
  js <- foldM appendToOutput "" files
  case only_js flags of
    True -> do
      putStr "Generating JavaScript ... "
      writeFile (buildPath flags rootFile "js") (genJs js)
      putStrLn "Done"
    False -> do
      putStr "Generating HTML ... "
      runtime <- getRuntime flags
      let html = genHtml $ createHtml runtime (takeBaseName rootFile) (sources js) moduleName ""
      writeFile (buildPath flags rootFile "html") html
      putStrLn "Done"

    where
      appendToOutput :: String -> FilePath -> IO String
      appendToOutput js filePath =
          do src <- readFile (elmo flags filePath)
             return (src ++ js)

      genHtml = if minify flags then Normal.renderHtml else Pretty.renderHtml
      genJs = if minify flags then BS.unpack . JS.minify . BS.pack else id
      sources js = map Link (scripts flags) ++
                   [ Source (if minify flags then Minified else Readable) js ]


buildFiles :: Flags -> Int -> Interfaces -> String -> [FilePath] -> IO (String, Interfaces)
buildFiles _ _ interfaces moduleName [] = return (moduleName, interfaces)
buildFiles flags numModules interfaces _ (filePath:rest) = do
  (name,interface) <- buildFile flags (numModules - length rest) numModules interfaces filePath
  let interfaces' = Map.insert name interface interfaces
  buildFiles flags numModules interfaces' name rest
