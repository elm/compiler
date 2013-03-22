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
docs = force . unsafePerformIO . safeReadDocs . getDataFileName $ "docs.json"

safeReadDocs :: IO FilePath -> IO String
safeReadDocs path = E.catch (readDocs path) (emitError path)

readDocs :: IO FilePath -> IO String
readDocs path = do
  name <- path
  exists <- doesFileExist name
  if exists then readFile name
            else ioError . userError $ "File Not Found"


emitError :: IO FilePath -> IOError -> IO String
emitError path err = do
    name <- path
    putStrLn $ "Error! Types for standard library not loaded properly!\n File should be here:" ++ name ++ "\n The file is created and copied by command: cabal install"
    putStrLn (show err)
    return "{\"modules\":[]}"
