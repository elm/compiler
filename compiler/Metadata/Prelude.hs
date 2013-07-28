module Metadata.Prelude (interfaces, add) where

import qualified Data.Map as Map
import qualified Control.Exception as E
import qualified Paths_Elm as Path
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import SourceSyntax.Module
import qualified Data.Binary as Binary


add :: Module t v -> Module t v
add (Module name exs ims stmts) = Module name exs (customIms ++ ims) stmts
    where
      customIms = concatMap addModule prelude

      addModule (n, method) = case lookup n ims of
                                Nothing     -> [(n, method)]
                                Just (As m) -> [(n, method)]
                                Just _      -> []

prelude :: [(String, ImportMethod)]
prelude = text : map (\n -> (n, Hiding [])) modules
  where
    text = ("Text", Hiding ["link", "color", "height"])
    modules = [ "Prelude", "Signal", "List", "Maybe", "Time"
              , "Graphics.Element", "Color", "Graphics.Collage" ]

{-# NOINLINE interfaces #-}
interfaces :: Interfaces
interfaces =
    unsafePerformIO (safeReadDocs =<< Path.getDataFileName "interfaces.data")

safeReadDocs :: FilePath -> IO Interfaces
safeReadDocs name =
    E.catch (readDocs name) $ \err -> do
      putStrLn $ unlines [ "Error reading types for standard library!"
                         , "  " ++ show (err :: IOError)
                         , "The file should be located here: " ++ name
                         , "  If you are using a stable version of Elm,"
                         , "  please report an issue at github.com/evancz/Elm"
                         , "  and specify your versions of Elm and your OS" ]
      exitFailure

readDocs :: FilePath -> IO Interfaces
readDocs name = do
  exists <- doesFileExist name
  if exists then Map.fromList `fmap` Binary.decodeFile name
            else ioError . userError $ "File Not Found"
