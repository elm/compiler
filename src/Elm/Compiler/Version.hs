{-# OPTIONS_GHC -Wall #-}
module Elm.Compiler.Version (version) where

import qualified Data.Version as Version
import qualified Paths_elm_compiler as This
import Elm.Package


rawVersion :: [Int]
rawVersion =
    Version.versionBranch This.version

version :: Version
version =
  case rawVersion of
    major : minor : patch : _ ->
        Version major minor patch

    [major, minor] ->
        Version major minor 0

    [major] ->
        Version major 0 0

    [] ->
        error "could not detect version of elm-compiler you are using"
