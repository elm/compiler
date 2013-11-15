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

import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy as L

import qualified Metadata.Prelude as Prelude
import qualified Transform.Canonicalize as Canonical
import SourceSyntax.Module
import Parse.Module (getModuleName)
import Initialize (buildFromSource, getSortedDependencies)
import Generate.JavaScript (jsModule)
import Generate.Html (createHtml, JSSource(..))
import qualified InterfaceSerialization as IS
import Paths_Elm

import SourceSyntax.PrettyPrint (pretty, variable)
import Text.PrettyPrint as P
import qualified Type.Type as Type
import qualified Type.Alias as Alias

data Flags =
    Flags { make :: Bool
          , files :: [FilePath]
          , runtime :: Maybe FilePath
          , only_js :: Bool
          , print_types :: Bool
          , scripts :: [FilePath]
          , no_prelude :: Bool
          , cache_dir :: FilePath
          , build_dir :: FilePath
          , src_dir :: [FilePath]
          }
    deriving (Data,Typeable,Show,Eq)

flags = Flags
  { files = def &= args &= typ "FILES"
  , make = False
           &= help "automatically compile dependencies."
  , only_js = False
              &= help "Compile only to JavaScript."
  , no_prelude = False
                 &= help "Do not import Prelude by default, used only when compiling standard libraries."
  , scripts = [] &= typFile
              &= help "Load JavaScript files in generated HTML. Files will be included in the given order."
  , runtime = Nothing &= typFile
              &= help "Specify a custom location for Elm's runtime system."
  , cache_dir = "cache" &= typFile
                &= help "Directory for files cached to make builds faster. Defaults to cache/ directory."
  , build_dir = "build" &= typFile
                &= help "Directory for generated HTML and JS files. Defaults to build/ directory."
  , src_dir = ["."] &= typFile
              &= help "Additional source directories besides project root. Searched when using --make"
  , print_types = False
                  &= help "Print out infered types of top-level definitions."
  } &= help "Compile Elm programs to HTML, CSS, and JavaScript."
    &= helpArg [explicit, name "help", name "h"]
    &= versionArg [explicit, name "version", name "v", summary (showVersion version)]
    &= summary ("The Elm Compiler " ++ showVersion version ++ ", (c) Evan Czaplicki 2011-2013")

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

buildFile :: Flags -> Int -> Int -> Interfaces -> FilePath -> IO (String, ModuleInterface)
buildFile flags moduleNum numModules interfaces filePath = do
  compiled <- alreadyCompiled
  if not compiled then compile else do
      bytes <- IS.loadInterface (elmi flags filePath)
      let binary = IS.interfaceDecode (elmi flags filePath) =<< bytes
      case IS.validVersion filePath =<< binary of
        Left err -> do
          hPutStrLn stderr err
          exitFailure

        Right (name, interface) -> do
          when (print_types flags) $
               printTypes interfaces
                          (iTypes interface)
                          (iAliases interface)
                          (iImports interface)

          return (name, interface)

    where
      alreadyCompiled :: IO Bool
      alreadyCompiled = do
        existsi <- doesFileExist (elmi flags filePath)
        existso <- doesFileExist (elmo flags filePath)
        if not existsi || not existso
            then return False
            else do tsrc <- getModificationTime filePath
                    tint <- getModificationTime (elmo flags filePath)
                    return (tsrc <= tint)

      number :: String
      number = "[" ++ show moduleNum ++ " of " ++ show numModules ++ "]"

      compile :: IO (String,ModuleInterface)
      compile = do
        source <- readFile filePath
        let name = case getModuleName source of
                     Just n -> n
                     Nothing -> "Main"
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
              Right modul -> return modul

        when (print_types flags)
                 (printTypes interfaces
                  (types metaModule) (aliases metaModule) (imports metaModule))

        let interface = Canonical.interface name $ ModuleInterface {
                          iVersion  = showVersion version,
                          iTypes    = types metaModule,
                          iImports  = imports metaModule,
                          iAdts     = datatypes metaModule,
                          iAliases  = aliases metaModule,
                          iFixities = fixities metaModule
                        }

        createDirectoryIfMissing True . dropFileName $ elmi flags filePath
        handle <- openBinaryFile (elmi flags filePath) WriteMode
        L.hPut handle (Binary.encode (name,interface))
        hClose handle
        writeFile (elmo flags filePath) (jsModule metaModule)
        return (name,interface)

printTypes interfaces moduleTypes moduleAliases moduleImports = do
  putStrLn ""
  let rules = Alias.rules interfaces moduleAliases moduleImports
  forM_ (Map.toList moduleTypes) $ \(n,t) -> do
      print $ variable n <+> P.text ":" <+> pretty (Alias.realias rules t)
  putStrLn ""

getRuntime :: Flags -> IO FilePath
getRuntime flags =
    case runtime flags of
      Just fp -> return fp
      Nothing -> getDataFileName "elm-runtime.js"

build :: Flags -> FilePath -> IO ()
build flags rootFile = do
  let noPrelude = no_prelude flags
  builtIns <- if noPrelude then return Map.empty else Prelude.interfaces

  files <- if make flags
             then getSortedDependencies (src_dir flags) builtIns rootFile
             else return [rootFile]

  (moduleName, interfaces) <- buildFiles flags (length files) builtIns "" files

  js <- foldM appendToOutput BS.empty files

  (extension, code) <- case only_js flags of
    True -> do
      putStr "Generating JavaScript ... "
      return ("js", js)
    False -> do
      putStr "Generating HTML ... "
      rtsPath <- getRuntime flags
      return ("html", BS.pack . renderHtml $
                    createHtml rtsPath (takeBaseName rootFile) (sources js) moduleName "")

  let targetFile = buildPath flags rootFile extension
  createDirectoryIfMissing True (takeDirectory targetFile)
  BS.writeFile targetFile code
  putStrLn "Done"

    where
      appendToOutput :: BS.ByteString -> FilePath -> IO BS.ByteString
      appendToOutput js filePath =
          do src <- BS.readFile (elmo flags filePath)
             return (BS.append src js)

      sources js = map Link (scripts flags) ++ [ Source js ]

buildFiles :: Flags -> Int -> Interfaces -> String -> [FilePath] -> IO (String, Interfaces)
buildFiles _ _ interfaces moduleName [] = return (moduleName, interfaces)
buildFiles flags numModules interfaces _ (filePath:rest) = do
  (name,interface) <- buildFile flags (numModules - length rest) numModules interfaces filePath
  let interfaces' = Map.insert name interface interfaces
  buildFiles flags numModules interfaces' name rest
