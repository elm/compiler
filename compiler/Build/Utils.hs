{-# OPTIONS_GHC -W #-}
module Build.Utils where

import Control.Monad.Error
import System.Directory (doesFileExist)
import System.Environment (getEnv)
import System.FilePath ((</>), (<.>))

import qualified Build.Flags as Flag
import qualified Build.Print as Print
import Build.SrcFile (SrcFile)
import qualified Build.SrcFile as SrcFile
import qualified Elm.Internal.Name as Name
import qualified Paths_Elm as This

buildPath :: Flag.Flags -> SrcFile a -> String -> FilePath
buildPath = flaggedPath Flag.build_dir

cachePath :: Flag.Flags -> SrcFile a -> String -> FilePath
cachePath = flaggedPath Flag.cache_dir

flaggedPath :: (Flag.Flags -> FilePath) -> Flag.Flags -> SrcFile a -> String -> FilePath
flaggedPath acc flags srcFile ext =
  acc flags </> pkgPath </> modul <.> ext
  where pkgPath = case SrcFile.pkg srcFile of
          Nothing -> ""
          Just name -> Name.toFilePath name
        modul = SrcFile.modulePath srcFile

elmo :: Flag.Flags -> SrcFile a -> FilePath
elmo flags filePath =
    cachePath flags filePath "elmo"

elmi :: Flag.Flags -> SrcFile a -> FilePath
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

run :: ErrorT String IO a -> IO a
run command = do
  either <- runErrorT command
  case either of
    Left err -> Print.failure err
    Right result -> return result
