{-# OPTIONS_GHC -W #-}
module Build.Utils where

import Control.Monad.Error
import System.Directory (doesFileExist)
import System.Environment (getEnv)
import System.FilePath ((</>), (<.>), replaceExtension)
import System.IO.Error (tryIOError)
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
      environment <- tryIOError (getEnv "ELM_HOME")
      case environment of
        Right env -> return (env </> name)
        Left _ ->
          fail $ unlines
            [ "Unable to find the ELM_HOME environment variable when searching"
            , "for the " ++ name ++ " file."
            , ""
            , "If you installed Elm Platform with the Mac or Windows installer, it looks like"
            , "ELM_HOME was not set automatically. Look up how to set environment variables"
            , "on your platform and set ELM_HOME to the directory that contains Elm's static"
            , "files:"
            , ""
            , "  * On Mac it is /usr/local/share/elm"
            , "  * On Windows it is one of the following:"
            , "      C:/Program Files/Elm Platform/0.12.3/share"
            , "      C:/Program Files (x86)/Elm Platform/0.12.3/share"
            , ""
            , "If it seems like a more complex issue, please report it here:"
            , "    <https://github.com/elm-lang/elm-platform/issues>"
            ]

run :: ErrorT String IO a -> IO a
run command = do
  either <- runErrorT command
  case either of
    Left err -> Print.failure err
    Right result -> return result
