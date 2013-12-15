module Build.Build (build) where

import Control.Monad (when, forM_, foldM)
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Binary as Binary
import System.Directory
import System.Exit
import System.FilePath
import System.IO

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy as L

import qualified Metadata.Prelude as Prelude
import qualified Transform.Canonicalize as Canonical
import SourceSyntax.Module
import Parse.Module (getModuleName)
import qualified Build.FromSource as FromSource
import Build.Dependencies (getSortedDependencies)
import Generate.JavaScript (jsModule)
import Generate.Html (createHtml, JSSource(..))
import qualified InterfaceSerialization as IS

import SourceSyntax.PrettyPrint (pretty, variable)
import Text.PrettyPrint as P
import qualified Type.Alias as Alias

import qualified Build.Utils as Utils
import qualified Build.Info as Info
import qualified Build.Flags as Flag

buildFile :: Flag.Flags -> Int -> Int -> Interfaces -> FilePath -> IO (String, ModuleInterface)
buildFile flags moduleNum numModules interfaces filePath = do
  compiled <- alreadyCompiled
  if not compiled then compile else do
      bytes <- IS.loadInterface (Utils.elmi flags filePath)
      let binary = IS.interfaceDecode (Utils.elmi flags filePath) =<< bytes
      case IS.validVersion filePath =<< binary of
        Left err -> do
          hPutStrLn stderr err
          exitFailure

        Right (name, interface) -> do
          when (Flag.print_types flags) $
               printTypes interfaces
                          (iTypes interface)
                          (iAliases interface)
                          (iImports interface)

          return (name, interface)

    where
      alreadyCompiled :: IO Bool
      alreadyCompiled = do
        existsi <- doesFileExist (Utils.elmi flags filePath)
        existso <- doesFileExist (Utils.elmo flags filePath)
        if not existsi || not existso
            then return False
            else do tsrc <- getModificationTime filePath
                    tint <- getModificationTime (Utils.elmo flags filePath)
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

        createDirectoryIfMissing True (Flag.cache_dir flags)
        createDirectoryIfMissing True (Flag.build_dir flags)

        metaModule <-
            case FromSource.build (Flag.no_prelude flags) interfaces source of
              Left errors -> do
                  mapM print (List.intersperse (P.text " ") errors)
                  exitFailure
              Right modul -> return modul

        when (Flag.print_types flags)
                 (printTypes interfaces
                  (types metaModule) (aliases metaModule) (imports metaModule))

        let interface = Canonical.interface name $ ModuleInterface {
                          iVersion  = Info.version,
                          iTypes    = types metaModule,
                          iImports  = imports metaModule,
                          iAdts     = datatypes metaModule,
                          iAliases  = aliases metaModule,
                          iFixities = fixities metaModule
                        }

        createDirectoryIfMissing True . dropFileName $ Utils.elmi flags filePath
        handle <- openBinaryFile (Utils.elmi flags filePath) WriteMode
        L.hPut handle (Binary.encode (name,interface))
        hClose handle
        writeFile (Utils.elmo flags filePath) (jsModule metaModule)
        return (name,interface)

printTypes interfaces moduleTypes moduleAliases moduleImports = do
  putStrLn ""
  let rules = Alias.rules interfaces moduleAliases moduleImports
  forM_ (Map.toList moduleTypes) $ \(n,t) -> do
      print $ variable n <+> P.text ":" <+> pretty (Alias.realias rules t)
  putStrLn ""

build :: Flag.Flags -> FilePath -> IO ()
build flags rootFile = do
  let noPrelude = Flag.no_prelude flags
  builtIns <- if noPrelude then return Map.empty else Prelude.interfaces

  files <- if Flag.make flags
             then getSortedDependencies (Flag.src_dir flags) builtIns rootFile
             else return [rootFile]

  (moduleName, interfaces) <- buildFiles flags (length files) builtIns "" files

  js <- foldM appendToOutput BS.empty files

  (extension, code) <- case Flag.only_js flags of
    True -> do
      putStr "Generating JavaScript ... "
      return ("js", js)
    False -> do
      putStr "Generating HTML ... "
      rtsPath <- case Flag.runtime flags of
                   Just fp -> return fp
                   Nothing -> Info.runtime
      return ("html", renderHtml $
                    createHtml rtsPath (takeBaseName rootFile) (sources js) moduleName "")

  let targetFile = Utils.buildPath flags rootFile extension
  createDirectoryIfMissing True (takeDirectory targetFile)
  BS.writeFile targetFile code
  putStrLn "Done"

    where
      appendToOutput :: BS.ByteString -> FilePath -> IO BS.ByteString
      appendToOutput js filePath =
          do src <- BS.readFile (Utils.elmo flags filePath)
             return (BS.append src js)

      sources js = map Link (Flag.scripts flags) ++ [ Source js ]

buildFiles :: Flag.Flags -> Int -> Interfaces -> String -> [FilePath] -> IO (String, Interfaces)
buildFiles _ _ interfaces moduleName [] = return (moduleName, interfaces)
buildFiles flags numModules interfaces _ (filePath:rest) = do
  (name,interface) <- buildFile flags (numModules - length rest) numModules interfaces filePath
  let interfaces' = Map.insert name interface interfaces
  buildFiles flags numModules interfaces' name rest
