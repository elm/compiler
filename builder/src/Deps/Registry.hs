{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Deps.Registry
  ( Registry(..)
  , KnownVersions(..)
  , read
  , fetch
  , update
  , latest
  , getVersions
  , getVersions'
  )
  where


import Prelude hiding (read)
import Control.Monad (liftM2)
import Data.Binary (Binary, get, put)
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Name as Name
import qualified Data.Utf8 as Utf8

import qualified Deps.Website as Website
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified File
import qualified Http
import qualified Json.Decode as D
import qualified Reporting.Exit as E
import qualified Reporting.Suggest as Suggest
import qualified Stuff



-- REGISTRY


data Registry =
  Registry
    { _count :: !Int
    , _versions :: !(Map.Map Pkg.Name KnownVersions)
    }


data KnownVersions =
  KnownVersions
    { _newest :: V.Version
    , _previous :: ![V.Version]
    }



-- READ


read :: Stuff.PackageCache -> IO (Maybe Registry)
read cache =
  do  let path = Stuff.registry cache
      exists <- File.exists path
      if exists
        then File.readBinary path
        else return Nothing



-- FETCH


fetch :: Http.Manager -> Stuff.PackageCache -> IO (Either E.RegistryProblem Registry)
fetch manager cache =
  post manager "/all-packages" allPkgsDecoder $
    \versions ->
      do  let size = Map.foldr' addEntry 0 versions
          let registry = Registry size versions
          let path = Stuff.registry cache
          File.writeBinary path registry
          return registry


addEntry :: KnownVersions -> Int -> Int
addEntry (KnownVersions _ vs) count =
  count + 1 + length vs


allPkgsDecoder :: D.Decoder () (Map.Map Pkg.Name KnownVersions)
allPkgsDecoder =
  do  pairs <- D.pairs (D.list (D.mapError (\_ -> ()) V.decoder))
      checkKeys pairs Map.empty
  where
    checkKeys pairs pkgs =
      case pairs of
        [] ->
          return pkgs

        (key, versions) : rest ->
          case Pkg.fromString key of
            Right pkg ->
              case List.sortBy (flip compare) versions of
                [] ->
                  D.failure ()

                v:vs ->
                  checkKeys rest (Map.insert pkg (KnownVersions v vs) pkgs)

            Left _ ->
              D.failure ()



-- UPDATE


update :: Http.Manager -> Stuff.PackageCache -> Registry -> IO (Either E.RegistryProblem Registry)
update manager cache oldRegistry@(Registry size packages) =
  post manager ("/all-packages/since/" ++ show size) (D.list newPkgDecoder) $
    \news ->
      case news of
        [] ->
          return oldRegistry

        _:_ ->
          let
            newSize = size + length news
            newPkgs = foldr addNew packages news
            newRegistry = Registry newSize newPkgs
          in
          do  File.writeBinary (Stuff.registry cache) newRegistry
              return newRegistry


addNew :: (Pkg.Name, V.Version) -> Map.Map Pkg.Name KnownVersions -> Map.Map Pkg.Name KnownVersions
addNew (name, version) versions =
  let
    add maybeKnowns =
      case maybeKnowns of
        Just (KnownVersions v vs) ->
          KnownVersions version (v:vs)

        Nothing ->
          KnownVersions version []
  in
  Map.alter (Just . add) name versions


newPkgDecoder :: D.Decoder () (Pkg.Name, V.Version)
newPkgDecoder =
  do  str <- D.string
      case Utf8.split 0x40 {-@-} str of
        [key, value] ->
          case Pkg.fromString key of
            Right pkg ->
              case V.fromString value of
                Just vsn -> return (pkg, vsn)
                Nothing -> D.failure ()

            Left _ ->
              D.failure ()

        _ ->
          D.failure ()



-- LATEST


latest :: Http.Manager -> Stuff.PackageCache -> IO (Either E.RegistryProblem Registry)
latest manager cache =
  do  maybeOldRegistry <- read cache
      case maybeOldRegistry of
        Just oldRegistry ->
          update manager cache oldRegistry

        Nothing ->
          fetch manager cache



-- GET VERSIONS


getVersions :: Pkg.Name -> Registry -> Maybe KnownVersions
getVersions name (Registry _ versions) =
  Map.lookup name versions


getVersions' :: Pkg.Name -> Registry -> Either [Pkg.Name] KnownVersions
getVersions' name (Registry _ versions) =
  case Map.lookup name versions of
    Just kvs -> Right kvs
    Nothing -> Left $ nearbyNames name (Map.keys versions)


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
  if possibility == "elm" || possibility == "elm-explorations"
  then 0
  else abs (Suggest.distance bad (Name.toChars possibility))



-- POST


post :: Http.Manager -> String -> D.Decoder x a -> (a -> IO b) -> IO (Either E.RegistryProblem b)
post manager path decoder callback =
  Http.post manager (Website.route path []) [] E.RP_Http $
    \body ->
      case D.fromByteString decoder body of
        Right a -> Right <$> callback a
        Left _ -> return $ Left E.RP_Data



-- BINARY


instance Binary Registry where
  get = liftM2 Registry get get
  put (Registry a b) = put a >> put b


instance Binary KnownVersions where
  get = liftM2 KnownVersions get get
  put (KnownVersions a b) = put a >> put b
