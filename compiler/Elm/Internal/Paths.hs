{-# OPTIONS_GHC -Wall #-}
module Elm.Internal.Paths where

import System.Directory (doesFileExist)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import qualified Paths_Elm as This

-- |Name of directory for all of a project's dependencies.
dependencyDirectory :: FilePath
dependencyDirectory = "elm_dependencies"

-- |Name of the dependency file, specifying dependencies and
--  other metadata for building and sharing projects.
dependencyFile :: FilePath
dependencyFile = "elm_dependencies.json"

{-# NOINLINE runtime #-}
-- |The absolute path to Elm's runtime system.
runtime :: FilePath
runtime = unsafePerformIO $ getDataFile "elm-runtime.js"

{-# NOINLINE docs #-}
-- |The absolute path to Elm's core library documentation.
docs :: FilePath
docs = unsafePerformIO $ getDataFile "docs.json"

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
