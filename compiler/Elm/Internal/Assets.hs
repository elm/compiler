{-# OPTIONS_GHC -Wall #-}
module Elm.Internal.Assets where

import Build.Utils (getDataFile)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

{-| Name of directory for all of a project's dependencies. -}
packagesDirectory :: FilePath
packagesDirectory = "elm_packages"

{-| Describes the exact versions of every library used for your project. This
information is written by elm-get when it solves and installs dependencies.
-}
solvedDependencies :: FilePath
solvedDependencies =
    packagesDirectory </> "solved-dependencies.json"

{-| Name of the dependency file, specifying dependencies and other metadata
for building and sharing projects.
-}
dependencyFile :: FilePath
dependencyFile =
    "elm_package.json"

{-| The absolute path to Elm's runtime system. -}
{-# NOINLINE runtime #-}
runtime :: FilePath
runtime =
    unsafePerformIO $ getDataFile "elm-runtime.js"

{-| The absolute path to Elm's core library documentation. -}
{-# NOINLINE docs #-}
docs :: FilePath
docs =
    unsafePerformIO $ getDataFile "docs.json"
