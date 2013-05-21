module LoadLibraries (docs) where

import Control.DeepSeq (force)
import qualified Control.Exception as E
import Paths_Elm
import System.Directory
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)

-- See stackoverflow discussion for trade-off between using unsafeIO or TemplateHaskell:
-- http://stackoverflow.com/questions/12716215/load-pure-global-variable-from-file

-- Given the awkwardness of including a compile-time generated file
-- vs loading static data, then the unsafeIO seems better.

{-# NOINLINE docs #-}
docs :: String
docs = force $ unsafePerformIO (safeReadDocs =<< getDataFileName "docs.json")

safeReadDocs :: FilePath -> IO String
safeReadDocs name = E.catch (readDocs name) (emitError name)

readDocs :: FilePath -> IO String
readDocs name = do
  exists <- doesFileExist name
  if exists then readFile name
            else ioError . userError $ "File Not Found"


emitError :: FilePath -> IOError -> IO String
emitError name err = do
    putStrLn $ "Error! Types for standard library not loaded properly!\n  File should be here:" ++ name ++ "\n  The file is created and copied by command: cabal install"
    putStrLn (show err)
    return "{\"modules\":[]}"
