module Metadata.Load (interfaces) where

import qualified Control.Exception as E
import Paths_Elm
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import SourceSyntax.Module
import Data.Binary

-- See stackoverflow discussion for trade-off between using unsafeIO or TemplateHaskell:
-- http://stackoverflow.com/questions/12716215/load-pure-global-variable-from-file

-- Given the awkwardness of including a compile-time generated file
-- vs loading static data, then the unsafeIO seems better.

{-# NOINLINE interfaces #-}
interfaces :: [ModuleInterface]
interfaces = unsafePerformIO (safeReadDocs =<< getDataFileName "interfaces.data")

safeReadDocs :: FilePath -> IO [ModuleInterface]
safeReadDocs name =
    E.catch (readDocs name) $ \err -> do
      putStrLn $ unlines [ "Error reading types for standard library!"
                         , "    The file should be located here: " ++ name
                         , "    If you are using a stable version of Elm,"
                         , "    please report an issue at github.com/evancz/Elm"
                         , "    and specify your versions of Elm and your OS" ]
      print (err :: IOError)
      exitFailure

readDocs :: FilePath -> IO [ModuleInterface]
readDocs name = do
  exists <- doesFileExist name
  if exists then decodeFile name
            else ioError . userError $ "File Not Found"
