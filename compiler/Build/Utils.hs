{-# OPTIONS_GHC -W #-}
module Build.Utils where

import System.Directory (doesFileExist)
import System.Environment (getEnv)
import System.FilePath ((</>), replaceExtension)
import qualified Build.Flags as Flag
import qualified Paths_Elm as This

buildPath :: Flag.Flags -> FilePath -> String -> FilePath
buildPath flags filePath ext =
    Flag.build_dir flags </> replaceExtension filePath ext

cachePath :: Flag.Flags -> FilePath -> String -> FilePath
cachePath flags filePath ext =
    Flag.cache_dir flags </> replaceExtension filePath ext

elmo :: Flag.Flags -> FilePath -> FilePath
elmo flags filePath =
    cachePath flags filePath "elmo"

elmi :: Flag.Flags -> FilePath -> FilePath
elmi flags filePath =
    cachePath flags filePath "elmi"

-- |The absolute path to a data file
getDataFile :: FilePath -> IO FilePath
getDataFile name = do
  path <- This.getDataFileName name
  exists <- doesFileExist path
  if exists
    then return path
    else do
      env <- getEnv "ELM_HOME"
      return (env </> name)
