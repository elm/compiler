{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Deps.Get
  ( all
  , AllPackages
  , Mode(..)
  , versions
  , info
  , docs
  )
  where

import Prelude hiding (all)
import Control.Monad.Except (catchError, liftIO)
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Elm.Docs as Docs
import qualified Elm.Package as Pkg
import Elm.Package (Name, Version)

import qualified Deps.Website as Website
import qualified Elm.Project.Json as Project
import qualified Elm.Utils as Utils
import qualified File.IO as IO
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Assets as E
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task
import qualified Json.Decode as Decode



-- ALL VERSIONS


data Mode = RequireLatest | AllowOffline


newtype AllPackages = AllPackages (Map Name [Version])


all :: Mode -> Task.Task AllPackages
all mode =
  do  dir <- Task.getPackageCacheDir
      let versionsFile = dir </> "versions.dat"
      exists <- liftIO $ Dir.doesFileExist versionsFile
      if exists
        then fetchNew versionsFile mode
        else fetchAll versionsFile


fetchAll :: FilePath -> Task.Task AllPackages
fetchAll versionsFile =
  do  packages <- Website.getAllPackages
      let size = Map.foldr ((+) . length) 0 packages
      IO.writeBinary versionsFile (size, packages)
      return (AllPackages packages)


fetchNew :: FilePath -> Mode -> Task.Task AllPackages
fetchNew versionsFile mode =
  do  (size, packages) <- IO.readBinary versionsFile

      news <-
        case mode of
          RequireLatest ->
            Website.getNewPackages size

          AllowOffline ->
            Website.getNewPackages size `catchError` \_ ->
              do  Task.report Progress.UnableToLoadLatestPackages
                  return []

      if null news
        then return (AllPackages packages)
        else
          do  let newAllPkgs = List.foldl' addNew packages news
              IO.writeBinary versionsFile (size + length news, newAllPkgs)
              return (AllPackages newAllPkgs)


addNew :: Map Name [Version] -> (Name, Version) -> Map Name [Version]
addNew packages (name, version) =
  Map.insertWith (++) name [version] packages



-- VERSIONS


versions :: Name -> AllPackages -> Either [Name] [Version]
versions name (AllPackages pkgs) =
  case Map.lookup name pkgs of
    Just vsns ->
      Right vsns

    Nothing ->
      Left (Utils.nearbyNames Pkg.toString name (Map.keys pkgs))



-- PACKAGE INFO


info :: Name -> Version -> Task.Task Project.PkgInfo
info name version =
  do  dir <- Task.getPackageCacheDirFor name version
      let elmJson = dir </> "elm.json"

      exists <- liftIO $ Dir.doesFileExist elmJson

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


docs :: Name -> Version -> Task.Task Docs.Documentation
docs name version =
  do  dir <- Task.getPackageCacheDirFor name version
      let docsJson = dir </> "docs.json"

      exists <- liftIO $ Dir.doesFileExist docsJson

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


errorToDocs :: Docs.Error -> [P.Doc]
errorToDocs err =
  let
    details =
      case err of
        Docs.BadAssociativity txt ->
          ["Binary","operators","cannot","have",P.red (P.text (show txt)),"as","their","associativity."]

        Docs.BadName ->
          ["It","is","not","a","valid","module","name."]

        Docs.BadType ->
          ["It","is","not","a","valid","Elm","type."]
  in
  details
  ++
  ["This","should","not","happen","in","general,","so","please","report","this","at"
  ,"<https://github.com/elm-lang/package.elm-lang.org/issues>"
  ,"if","you","think","it","is","on","the","Elm","side!"
  ]
