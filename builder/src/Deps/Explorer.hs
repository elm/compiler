{-# OPTIONS_GHC -Wall #-}
module Deps.Explorer
  ( Explorer
  , Metadata
  , Info(..)
  , run
  , exists
  , getVersions
  , getConstraints
  )
  where

{-| It is expensive to load ALL package metadata. You would need to:

  1. Know all the packages and all their versions.
  2. Download or read the elm.json file for each version.

The goal of this module is to only pay for (1) and pay for (2) as needed.
-}


import Control.Monad.Except (liftIO, lift, throwError)
import Control.Monad.State (StateT, evalStateT, gets, modify)
import Data.Map (Map)
import qualified Data.Map as Map

import Elm.Package (Name, Version)

import qualified Deps.Cache as Cache
import qualified Elm.PerUserCache as PerUserCache
import qualified Elm.Project.Json as Project
import Elm.Project.Constraint (Constraint)
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Deps as E
import qualified Reporting.Task as Task



-- EXPLORER


type Explorer =
  StateT Metadata Task.Task


data Metadata =
  Metadata
    { _registry :: Cache.PackageRegistry
    , _info :: Map (Name, Version) Info
    }


data Info =
  Info
    { _elm :: Constraint
    , _pkgs :: Map Name Constraint
    }


run :: Cache.PackageRegistry -> Explorer a -> Task.Task a
run registry explorer =
  evalStateT explorer (Metadata registry Map.empty)



-- EXISTS


exists :: Name -> Explorer ()
exists name =
  do  registry <- gets _registry
      case Cache.getVersions name registry of
        Right _ ->
          return ()

        Left suggestions ->
          throwError (Exit.Deps (E.PackageNotFound name suggestions))



-- VERSIONS


getVersions :: Name -> Explorer [Version]
getVersions name =
  do  registry <- gets _registry
      case Cache.getVersions name registry of
        Right versions ->
          return versions

        Left _suggestions ->
          do  elmHome <- liftIO PerUserCache.getElmHome
              throwError (Exit.Deps (E.CorruptVersionCache elmHome name))



-- CONSTRAINTS


getConstraints :: Name -> Version -> Explorer Info
getConstraints name version =
  do  allInfo <- gets _info
      case Map.lookup (name, version) allInfo of
        Just info ->
          return info

        Nothing ->
          do  pkgInfo <- lift $ Cache.getElmJson name version

              let elm = Project._pkg_elm_version pkgInfo
              let pkgs = Project._pkg_deps pkgInfo
              let info = Info elm pkgs

              modify $ \(Metadata vsns infos) ->
                Metadata vsns $ Map.insert (name, version) info infos

              return info
