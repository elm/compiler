{-# OPTIONS_GHC -Wall #-}
module Elm.PerUserCache
  ( getPackageRoot
  , getReplRoot
  )
  where

import qualified System.Directory as Dir
import qualified System.Environment as Env
import System.FilePath ((</>))

import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg



-- ROOTS


getPackageRoot :: IO FilePath
getPackageRoot =
  getRoot "package"


getReplRoot :: IO FilePath
getReplRoot =
  getRoot "repl"


getRoot :: FilePath -> IO FilePath
getRoot projectName =
  do  maybeHome <- Env.lookupEnv "ELM_HOME"
      home <- maybe (Dir.getAppUserDataDirectory "elm") return maybeHome
      let root = home </> version </> projectName
      Dir.createDirectoryIfMissing True root
      return root


version :: FilePath
version =
  Pkg.versionToString Compiler.version
