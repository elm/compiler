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
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text as Text
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Elm.Docs as Docs
import qualified Elm.Package as Pkg

import qualified Deps.Website as Website
import qualified Elm.Project.Json as Project
import qualified File.IO as IO
import qualified Reporting.Suggest as Suggest
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Assets as E
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task
import qualified Json.Decode as Decode



-- ALL VERSIONS


data Mode = RequireLatest | AllowOffline


newtype AllPackages = AllPackages (Map Pkg.Name [Pkg.Version])


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


addNew :: Map Pkg.Name [Pkg.Version] -> (Pkg.Name, Pkg.Version) -> Map Pkg.Name [Pkg.Version]
addNew packages (name, version) =
  Map.insertWith (++) name [version] packages



-- VERSIONS


versions :: Pkg.Name -> AllPackages -> Either [Pkg.Name] [Pkg.Version]
versions name (AllPackages pkgs) =
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
  if possibility == "elm-lang" || possibility == "elm-explorations" then
    0
  else
    abs (Suggest.distance bad (Text.unpack possibility))



-- PACKAGE INFO


info :: Pkg.Name -> Pkg.Version -> Task.Task Project.PkgInfo
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


docs :: Pkg.Name -> Pkg.Version -> Task.Task Docs.Documentation
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
