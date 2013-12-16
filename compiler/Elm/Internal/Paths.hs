module Elm.Internal.Paths where

import System.IO.Unsafe
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
runtime = unsafePerformIO $ This.getDataFileName "elm-runtime.js"

{-# NOINLINE docs #-}
-- |The absolute path to Elm's core library documentation.
docs :: FilePath
docs = unsafePerformIO $ This.getDataFileName "docs.json"
