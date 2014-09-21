{-# OPTIONS_GHC -W #-}
module Build.Utils where

import Control.Monad.Error
import System.Directory (doesFileExist)
import System.Environment (getEnv)
import System.FilePath ((</>), replaceExtension)
import System.IO.Error (tryIOError)
import qualified Build.Flags as Flag
import qualified Build.Print as Print
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
      environment <- tryIOError (getEnv "ELM_HOME")
      case environment of
        Right env -> return (env </> "compiler" </> name)
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
            , "      C:/Program Files/Elm Platform/0.13/share"
            , "      C:/Program Files (x86)/Elm Platform/0.13/share"
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
