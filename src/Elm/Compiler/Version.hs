{-# OPTIONS_GHC -Wall #-}
module Elm.Compiler.Version (version, rawVersion) where

import qualified Data.Version as Version
import qualified Paths_elm_compiler as This


version :: String
version =
    Version.showVersion This.version


rawVersion :: [Int]
rawVersion =
    Version.versionBranch This.version