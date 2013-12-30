module Build.File (build) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.RWS.Strict
import qualified Data.Binary as Binary
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Monoid (Last(..))
import qualified Data.Map as Map
import System.Directory
import System.Exit
import System.FilePath
import System.IO

import qualified Transform.Canonicalize as Canonical
import qualified Data.ByteString.Lazy as L

import qualified Build.Utils as Utils
import qualified Build.Flags as Flag
import qualified Build.Source as Source
import qualified Build.Print as Print
import qualified Generate.JavaScript as JS
import qualified InterfaceSerialization as IS
import qualified Parse.Module as Parser
import qualified SourceSyntax.Module as M

-- Reader: Runtime flags, always accessible
-- Writer: Remember the last module to be accessed
-- State:  Build up a map of the module interfaces
type BuildT m a = RWST Flag.Flags (Last String) M.Interfaces m a
type Build a = BuildT IO a

evalBuild :: Flag.Flags -> M.Interfaces -> Build () -> IO (Maybe String)
evalBuild fs is b = do
  (_, s) <- evalRWST b fs is
  return . getLast $ s

-- Builds a list of files, returning the moduleName of the last one.
-- Returns "" if the list is empty
build :: Flag.Flags -> M.Interfaces -> [FilePath] -> IO String
build flags is = fmap (Maybe.fromMaybe "") . evalBuild flags is . buildAll

buildAll :: [FilePath] -> Build ()
buildAll fs = mapM_ build1 (zip [1..] fs)
  where build1 (num, fname) = do
          compiled <- alreadyCompiled filePath
          case compiled of
            False ->
              let number = join ["[", show num, " of ", show total, "]"]
              compile number filePath
            True  -> retrieve filePath
        total = length fs

alreadyCompiled :: FilePath -> Build Bool
alreadyCompiled filePath = do
  flags <- ask
  liftIO $ do
    existsi <- doesFileExist (Utils.elmi flags filePath)
    existso <- doesFileExist (Utils.elmo flags filePath)
    if not existsi || not existso
      then return False
      else do tsrc <- getModificationTime filePath
              tint <- getModificationTime (Utils.elmo flags filePath)
              return (tsrc <= tint)

retrieve :: FilePath -> Build ()
retrieve filePath = do
  flags <- ask
  let elmi = Utils.elmi flags filePath
  bytes <- liftIO $ IS.loadInterface elmi
  let binary = IS.interfaceDecode elmi =<< bytes
  case IS.validVersion filePath =<< binary of
    Right (name, interface) ->
      do interfaces <- get
         liftIO $ when (Flag.print_types flags) (Print.interfaceTypes interfaces interface)
         update name interface

    Left err ->
      liftIO $ hPutStrLn stderr err >> exitFailure

compile :: String -> FilePath -> Build ()
compile number filePath =
  do flags      <- ask
     interfaces <- get
     source <- liftIO $ readFile filePath
     let name = getName source
     liftIO $ do
       printStatus name
       createDirectoryIfMissing True (Flag.cache_dir flags)
       createDirectoryIfMissing True (Flag.build_dir flags)

     metaModule <- 
       liftIO $ case Source.build (Flag.no_prelude flags) interfaces source of
           Right modul -> return modul
           Left errors -> do Print.errors errors
                             exitFailure

     liftIO $ when (Flag.print_types flags) $ Print.metaTypes interfaces metaModule
  
     let intermediate = (name, Canonical.interface name $ M.metaToInterface metaModule)
     generateCache intermediate metaModule
     uncurry update intermediate

  where
    getName source = case Parser.getModuleName source of
                       Just n -> n
                       Nothing -> "Main"

    printStatus name =
        hPutStrLn stdout $ concat [ number, " Compiling ", name
                                  , replicate (max 1 (20 - length name)) ' '
                                  , "( " ++ filePath ++ " )" ]

    generateCache intermediate metaModule = do
      flags <- ask
      liftIO $ do
        createDirectoryIfMissing True . dropFileName $ Utils.elmi flags filePath
        writeFile (Utils.elmo flags filePath) (JS.generate metaModule)
        withBinaryFile (Utils.elmi flags filePath) WriteMode $ \handle ->
          L.hPut handle (Binary.encode intermediate)

update :: String -> M.ModuleInterface -> Build ()
update name inter = modify (Map.insert name inter)
                    >> tell (Last . Just $ name)
