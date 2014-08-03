{-# OPTIONS_GHC -W #-}
module Build.Utils where

import Control.Monad.Error
import System.Directory (doesFileExist)
import System.Environment (getEnv)
import System.FilePath ((</>), (<.>))

import qualified Build.Flags as Flag
import qualified Build.Metadata as Metadata
import qualified Build.Print as Print
import qualified Elm.Internal.Name as N
import qualified Paths_Elm as This

path :: Metadata.Metadata a -> String -> FilePath
path metadata fileExtension =
    case Metadata._pkg metadata of
      Nothing -> filePath
      Just name -> N.toFilePath name </> filePath
    where
      filePath =
          foldr1 (</>) (Metadata._name metadata) <.> fileExtension

docsPath :: Metadata.Metadata a -> FilePath
docsPath metadata =
    "docs" </> path metadata "json"

buildPath :: Flag.Flags -> Metadata.Metadata a -> String -> FilePath
buildPath flags metadata fileExtension =
    Flag.build_dir flags </> path metadata fileExtension

cachePath :: Flag.Flags -> Metadata.Metadata a -> String -> FilePath
cachePath flags metadata fileExtension =
    Flag.cache_dir flags </> path metadata fileExtension

elmo :: Flag.Flags -> Metadata.Metadata a -> FilePath
elmo flags metadata =
    cachePath flags metadata "elmo"

elmi :: Flag.Flags -> Metadata.Metadata a -> FilePath
elmi flags metadata =
    cachePath flags metadata "elmi"

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

run :: ErrorT String IO a -> IO a
run command = do
  either <- runErrorT command
  case either of
    Left err -> Print.failure err
    Right result -> return result
