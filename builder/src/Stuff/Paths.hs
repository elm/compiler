{-# OPTIONS_GHC -Wall #-}
module Stuff.Paths
  ( docs
  , summary
  , prepublishDir
  , removeStuff
  , elmi
  , elmo
  , moduleDocs
  , temp
  )
  where


import Control.Monad.Trans (liftIO)
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))

import qualified Elm.Compiler as Compiler
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Reporting.Task as Task



-- PATHS


stuff :: FilePath
stuff =
  "elm-stuff" </> Pkg.versionToString Compiler.version


docs :: FilePath
docs =
  stuff </> "docs.json"


summary :: FilePath
summary =
  stuff </> "summary.dat"


prepublishDir :: FilePath
prepublishDir  =
  stuff </> "prepublish"



-- REMOVE STUFF


removeStuff :: FilePath -> Task.Task_ e ()
removeStuff root =
  liftIO $
  do  let dir = root </> "elm-stuff"
      exists <- Dir.doesDirectoryExist dir
      if exists
        then Dir.removeDirectoryRecursive dir
        else return ()



-- ELMI and ELMO


elmi :: FilePath -> ModuleName.Raw -> FilePath
elmi root name =
  toArtifactPath root name "elmi"


elmo :: FilePath -> ModuleName.Raw -> FilePath
elmo root name =
  toArtifactPath root name "elmo"


moduleDocs :: FilePath -> ModuleName.Raw -> FilePath
moduleDocs root name =
  toArtifactPath root name "json"


toArtifactPath :: FilePath -> ModuleName.Raw -> String -> FilePath
toArtifactPath root name ext =
  root </> stuff </> ModuleName.nameToHyphenPath name <.> ext



-- TEMP


temp :: String -> FilePath
temp ext =
  stuff </> "temp" <.> ext
