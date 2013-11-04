module Metadata.Prelude (interfaces, add) where

import qualified Data.Map as Map
import qualified Control.Exception as E
import qualified Paths_Elm as Path
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import SourceSyntax.Module
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BS
import Data.Either.Unwrap (fromRight)

import qualified InterfaceSerialization as IS

add :: Module t v -> Module t v
add (Module name exs ims stmts) = Module name exs (customIms ++ ims) stmts
    where
      customIms = concatMap addModule prelude

      addModule (n, method) = case lookup n ims of
                                Nothing     -> [(n, method)]
                                Just (As m) -> [(n, method)]
                                Just _      -> []

prelude :: [(String, ImportMethod)]
prelude = text ++ map (\n -> (n, Hiding [])) modules
  where
    text = map ((,) "Text") [ As "Text", Hiding ["link", "color", "height"] ]
    modules = [ "Basics", "Signal", "List", "Maybe", "Time", "Prelude"
              , "Graphics.Element", "Color", "Graphics.Collage" ]

{-# NOINLINE interfaces #-}
interfaces :: Interfaces
interfaces =
    unsafePerformIO (safeReadDocs =<< Path.getDataFileName "interfaces.data")

safeReadDocs :: FilePath -> IO Interfaces
safeReadDocs name =
    E.catch (readDocs name) $ \err -> do
      let _ = err :: IOError
      putStrLn $ unlines [ "Error reading types for standard library!"
                         , "    The file should be at " ++ name
                         , "    If you are using a stable version of Elm,"
                         , "    please report an issue at github.com/evancz/Elm"
                         , "    and specify your versions of Elm and your OS" ]
      exitFailure

firstModuleInterface :: Interfaces -> Either String (String, ModuleInterface)
firstModuleInterface interfaces =
    if Map.null interfaces then
        Left "No interfaces found in serialized Prelude!"
    else
        Right $ head $ Map.toList interfaces

readDocs :: FilePath -> IO Interfaces
readDocs filePath = do
  bytes <- IS.loadInterface filePath
  let interfaces = bytes >>= IS.interfaceDecode filePath

  -- Although every ModuleInterface that is deserialized in this collection
  -- contains the compiler version, we only check the first ModuleInterface
  -- since it doesn't make sense that different modules in Prelude would have
  -- been compiled by different compiler versions.
  case interfaces >>= firstModuleInterface >>= IS.validVersion filePath of
    Left err -> do
      hPutStrLn stderr err
      exitFailure

    -- Unwrapping the Right value here is safe since the whole above chain
    -- returns a Right value. The toList/fromList is necessary because of a
    -- problem with looking up keys in the Map after deserialization. Example
    -- at https://gist.github.com/jsl/7294493.
    Right _ -> return $ Map.fromList $ Map.toList $ fromRight interfaces
