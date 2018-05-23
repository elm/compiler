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
import Data.Map (Map)
import qualified Data.Text as Text
import System.FilePath ((</>))

import qualified Elm.Docs as Docs
import qualified Elm.Package as Pkg

import qualified Deps.Website as Website
import qualified Elm.Project.Json as Project
import qualified File.IO as IO
import qualified Json.Decode as Decode
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Assets as E
import qualified Reporting.Progress as Progress
import qualified Reporting.Suggest as Suggest
import qualified Reporting.Task as Task



-- PACKAGE REGISTRY


data PackageRegistry =
  PackageRegistry Int (Map Pkg.Name [Pkg.Version])


getPackageRegistry :: Task.Task PackageRegistry
getPackageRegistry =
  do  path <- getRegistryPath
      exists <- IO.exists path
      if exists
        then IO.readBinary path
        else rebuild


getRegistryPath :: Task.Task FilePath
getRegistryPath =
  do  dir <- Task.getPackageCacheDir
      return (dir </> "versions.dat")



-- REBUILD


rebuild :: Task.Task PackageRegistry
rebuild =
  do  path <- getRegistryPath
      packages <- Website.getAllPackages
      let size = Map.foldr ((+) . length) 0 packages
      let registry = PackageRegistry size packages
      IO.writeBinary path registry
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
      exists <- IO.exists path
      if not exists
        then rebuild
        else
          do  registry <- IO.readBinary path
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
              IO.writeBinary path newRegistry
              return newRegistry


addNew :: Map Pkg.Name [Pkg.Version] -> (Pkg.Name, Pkg.Version) -> Map Pkg.Name [Pkg.Version]
addNew packages (name, version) =
  Map.insertWith (++) name [version] packages



-- GET VERSIONS


getVersions :: Pkg.Name -> PackageRegistry -> Either [Pkg.Name] [Pkg.Version]
getVersions name (PackageRegistry _ pkgs) =
  case Map.lookup name pkgs of
    Just vsns ->
      Right vsns

    Nothing ->
      Left (nearbyNames name (Map.keys pkgs))


nearbyNames :: Pkg.Name -> [Pkg.Name] -> [Pkg.Name]
nearbyNames (Pkg.Name author1 project1) possibleNames =
  let
    authorDist = authorDistance (Text.unpack author1)
    projectDist = projectDistance (Text.unpack project1)

    addDistance name@(Pkg.Name author2 project2) =
      ( authorDist author2 + projectDist project2, name )
  in
  map snd $ take 4 $
    List.sortBy (compare `on` fst) $
      map addDistance possibleNames


authorDistance :: String -> Text.Text -> Int
authorDistance bad possibility =
  abs (Suggest.distance bad (Text.unpack possibility))


projectDistance :: String -> Text.Text -> Int
projectDistance bad possibility =
  if possibility == "elm" || possibility == "elm-explorations" then
    0
  else
    abs (Suggest.distance bad (Text.unpack possibility))



-- PACKAGE INFO


getElmJson :: Pkg.Name -> Pkg.Version -> Task.Task Project.PkgInfo
getElmJson name version =
  do  dir <- Task.getPackageCacheDirFor name version
      let elmJson = dir </> "elm.json"

      exists <- IO.exists elmJson

      json <-
        if exists
          then liftIO (BS.readFile elmJson)
          else
            do  bits <- Website.getElmJson name version
                liftIO (BS.writeFile elmJson bits)
                return bits

      case Decode.parse "project" E.badContentToDocs Project.pkgDecoder json of
        Right pkgInfo ->
          return pkgInfo

        Left _ ->
          do  IO.remove elmJson
              Task.throw $ Exit.Assets $ E.CorruptElmJson name version



-- DOCS


getDocs :: Pkg.Name -> Pkg.Version -> Task.Task Docs.Documentation
getDocs name version =
  do  dir <- Task.getPackageCacheDirFor name version
      let docsJson = dir </> "docs.json"

      exists <- IO.exists docsJson

      json <-
        if exists
          then liftIO (BS.readFile docsJson)
          else
            do  bits <- Website.getDocs name version
                liftIO (BS.writeFile docsJson bits)
                return bits

      case Decode.parse "docs" errorToDocs (Docs.toDict <$> Decode.list Docs.decoder) json of
        Right pkgInfo ->
          return pkgInfo

        Left _ ->
          do  IO.remove docsJson
              Task.throw $ Exit.Assets $ E.CorruptDocumentation name version


errorToDocs :: Docs.Error -> [D.Doc]
errorToDocs err =
  let
    details =
      case err of
        Docs.BadAssociativity txt ->
          ["Binary","operators","cannot","have",D.red (D.fromString (show txt)),"as","their","associativity."]

        Docs.BadName ->
          ["It","is","not","a","valid","module","name."]

        Docs.BadType ->
          ["It","is","not","a","valid","Elm","type."]
  in
  details
  ++
  ["This","should","not","happen","in","general,","so","please","report","this","at"
  ,"<https://github.com/elm/package.elm-lang.org/issues>"
  ,"if","you","think","it","is","on","the","Elm","side!"
  ]



-- BINARY


instance Binary PackageRegistry where
  get = liftM2 PackageRegistry get get
  put (PackageRegistry a b) = put a >> put b
