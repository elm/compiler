{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Deps.Cache
  ( optionalUpdate
  , mandatoryUpdate
  , PackageRegistry
  , getPackageRegistry
  , getVersions
  , getElmJson
  , getDocs
  )
  where


import Prelude hiding (all)
import Control.Monad (liftM2)
import Control.Monad.Except (catchError, liftIO)
import Data.Binary (Binary, get, put)
import qualified Data.ByteString as BS
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Name as Name
import System.FilePath ((</>))

import qualified Deps.Website as Website
import qualified Elm.Docs as Docs
import qualified Elm.Package as Pkg
import qualified Elm.Project.Json as Project
import qualified Elm.Version as V
import qualified File.IO as IO
import qualified Json.Decode as Decode
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Assets as E
import qualified Reporting.Progress as Progress
import qualified Reporting.Suggest as Suggest
import qualified Reporting.Task as Task



-- PACKAGE REGISTRY


data PackageRegistry =
  PackageRegistry Int (Map.Map Pkg.Name [V.Version])


getPackageRegistry :: Task.Task PackageRegistry
getPackageRegistry =
  do  path <- getRegistryPath
      exists <- liftIO $ IO.exists path
      if exists
        then unsafeGetRegistry path
        else rebuild


getRegistryPath :: Task.Task FilePath
getRegistryPath =
  do  dir <- Task.getPackageCacheDir
      return (dir </> "versions.dat")


unsafeGetRegistry :: FilePath -> Task.Task PackageRegistry
unsafeGetRegistry path =
  do  maybeRegistry <- liftIO $ IO.readBinary path
      case maybeRegistry of
        Just registry ->
          return registry

        Nothing ->
          liftIO (putStrLn path) >>
          Task.throw (error "TODO corrupt binary 3")



-- REBUILD


rebuild :: Task.Task PackageRegistry
rebuild =
  do  path <- getRegistryPath
      packages <- Website.getAllPackages
      let size = Map.foldr ((+) . length) 0 packages
      let registry = PackageRegistry size packages
      liftIO $ IO.writeBinary path registry
      return registry



-- UPDATE


optionalUpdate :: Task.Task PackageRegistry
optionalUpdate =
  update False


mandatoryUpdate :: Task.Task PackageRegistry
mandatoryUpdate =
  update True


update :: Bool -> Task.Task PackageRegistry
update isMandatory =
  do  path <- getRegistryPath
      exists <- liftIO $ IO.exists path
      if not exists
        then rebuild
        else
          do  registry <- unsafeGetRegistry path
              if isMandatory
                then attemptUpdate registry
                else attemptUpdate registry `catchError` recoverUpdate registry


recoverUpdate :: PackageRegistry -> a -> Task.Task PackageRegistry
recoverUpdate registry _ =
  do  Task.report Progress.UnableToLoadLatestPackages
      return registry


attemptUpdate :: PackageRegistry -> Task.Task PackageRegistry
attemptUpdate oldRegistry@(PackageRegistry size packages) =
  do  news <- Website.getNewPackages size
      case news of
        [] ->
          return oldRegistry

        _:_ ->
          let
            newSize = size + length news
            newPkgs = List.foldl' addNew packages news
            newRegistry = PackageRegistry newSize newPkgs
          in
          do  path <- getRegistryPath
              liftIO $ IO.writeBinary path newRegistry
              return newRegistry


addNew :: Map.Map Pkg.Name [V.Version] -> (Pkg.Name, V.Version) -> Map.Map Pkg.Name [V.Version]
addNew packages (name, version) =
  Map.insertWith (++) name [version] packages



-- GET VERSIONS


getVersions :: Pkg.Name -> PackageRegistry -> Either [Pkg.Name] [V.Version]
getVersions name (PackageRegistry _ pkgs) =
  case Map.lookup name pkgs of
    Just vsns ->
      Right vsns

    Nothing ->
      Left (nearbyNames name (Map.keys pkgs))


nearbyNames :: Pkg.Name -> [Pkg.Name] -> [Pkg.Name]
nearbyNames (Pkg.Name author1 project1) possibleNames =
  let
    authorDist = authorDistance (Name.toChars author1)
    projectDist = projectDistance (Name.toChars project1)

    addDistance name@(Pkg.Name author2 project2) =
      ( authorDist author2 + projectDist project2, name )
  in
  map snd $ take 4 $
    List.sortBy (compare `on` fst) $
      map addDistance possibleNames


authorDistance :: String -> Name.Name -> Int
authorDistance bad possibility =
  abs (Suggest.distance bad (Name.toChars possibility))


projectDistance :: String -> Name.Name -> Int
projectDistance bad possibility =
  if possibility == "elm" || possibility == "elm-explorations" then
    0
  else
    abs (Suggest.distance bad (Name.toChars possibility))



-- PACKAGE INFO


getElmJson :: Pkg.Name -> V.Version -> Task.Task Project.PkgInfo
getElmJson name version =
  do  dir <- Task.getPackageCacheDirFor name version
      let elmJson = dir </> "elm.json"

      exists <- liftIO $ IO.exists elmJson

      json <-
        if exists
          then liftIO (BS.readFile elmJson)
          else
            do  bits <- Website.getElmJson name version
                liftIO (BS.writeFile elmJson bits)
                return bits

      case Decode.fromByteString Project.pkgDecoder json of
        Right pkgInfo ->
          return pkgInfo

        Left _ ->
          do  liftIO $ IO.remove elmJson
              Task.throw $ Exit.Assets $ E.CorruptElmJson name version



-- DOCS


getDocs :: Pkg.Name -> V.Version -> Task.Task Docs.Documentation
getDocs name version =
  do  dir <- Task.getPackageCacheDirFor name version
      let docsJson = dir </> "docs.json"

      exists <- liftIO $ IO.exists docsJson

      json <-
        if exists
          then liftIO (BS.readFile docsJson)
          else
            do  bits <- Website.getDocs name version
                liftIO (BS.writeFile docsJson bits)
                return bits

      case Decode.fromByteString (Docs.toDict <$> Decode.list Docs.decoder) json of
        Right pkgInfo ->
          return pkgInfo

        Left _ ->
          do  liftIO $ IO.remove docsJson
              Task.throw $ Exit.Assets $ E.CorruptDocumentation name version



-- BINARY


instance Binary PackageRegistry where
  get = liftM2 PackageRegistry get get
  put (PackageRegistry a b) = put a >> put b
