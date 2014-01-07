{-# OPTIONS_GHC -W #-}
module Build.File (build) where

import Control.Monad (when)
import qualified Data.Binary as Binary
import qualified Data.Map as Map
import System.Directory
import System.Exit
import System.FilePath
import System.IO

import qualified Transform.Canonicalize as Canonical

import qualified Data.ByteString.Lazy as L

import qualified Build.Utils as Utils
import qualified Build.Flags as Flag
import qualified Build.Interface as Interface
import qualified Build.Print as Print
import qualified Build.Source as Source
import qualified Generate.JavaScript as JS
import qualified Parse.Module as Parser
import qualified SourceSyntax.Module as M

build :: Flag.Flags -> Int -> M.Interfaces -> String -> [FilePath]
      -> IO (String, M.Interfaces)
build _ _ interfaces moduleName [] =
    return (moduleName, interfaces)
build flags numModules interfaces _ (filePath:rest) =
    do (name,interface) <-
           build1 flags (numModules - length rest) numModules interfaces filePath
       let interfaces' = Map.insert name interface interfaces
       build flags numModules interfaces' name rest


build1 :: Flag.Flags -> Int -> Int -> M.Interfaces -> FilePath
       -> IO (String, M.ModuleInterface)
build1 flags moduleNum numModules interfaces filePath =
    do compiled <- alreadyCompiled flags filePath
       case compiled of
         False -> compile flags number interfaces filePath
         True  -> retrieve flags interfaces filePath
    where
      number = "[" ++ show moduleNum ++ " of " ++ show numModules ++ "]"


alreadyCompiled :: Flag.Flags -> FilePath -> IO Bool
alreadyCompiled flags filePath = do
  existsi <- doesFileExist (Utils.elmi flags filePath)
  existso <- doesFileExist (Utils.elmo flags filePath)
  if not existsi || not existso
    then return False
    else do tsrc <- getModificationTime filePath
            tint <- getModificationTime (Utils.elmo flags filePath)
            return (tsrc <= tint)

retrieve :: Flag.Flags -> Map.Map String M.ModuleInterface -> FilePath
         -> IO (String, M.ModuleInterface)
retrieve flags interfaces filePath = do
  iface <- Interface.load (Utils.elmi flags filePath)
  case Interface.isValid filePath iface of
    Right (name, interface) ->
        do when (Flag.print_types flags) (Print.interfaceTypes interfaces interface)
           return (name, interface)
    Left err ->
        do hPutStrLn stderr err
           exitFailure

compile :: Flag.Flags -> String -> M.Interfaces -> FilePath
        -> IO (String, M.ModuleInterface)
compile flags number interfaces filePath =
  do source <- readFile filePath
     let name = getName source
     printStatus name

     createDirectoryIfMissing True (Flag.cache_dir flags)
     createDirectoryIfMissing True (Flag.build_dir flags)

     metaModule <-
         case Source.build (Flag.no_prelude flags) interfaces source of
           Right modul -> return modul
           Left errors -> do Print.errors errors
                             exitFailure

     when (Flag.print_types flags) $ Print.metaTypes interfaces metaModule

     let intermediate = (name, Canonical.interface name $ M.metaToInterface metaModule)
     generateCache intermediate metaModule
     return intermediate

  where
    getName source = case Parser.getModuleName source of
                       Just n -> n
                       Nothing -> "Main"

    printStatus name =
        hPutStrLn stdout $ concat [ number, " Compiling ", name
                                  , replicate (max 1 (20 - length name)) ' '
                                  , "( " ++ filePath ++ " )" ]

    generateCache intermediate metaModule = do
      createDirectoryIfMissing True . dropFileName $ Utils.elmi flags filePath
      writeFile (Utils.elmo flags filePath) (JS.generate metaModule)
      withBinaryFile (Utils.elmi flags filePath) WriteMode $ \handle ->
          L.hPut handle (Binary.encode intermediate)
