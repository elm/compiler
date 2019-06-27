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
import qualified Data.List as List
import qualified Data.Map.Strict as Map

import qualified Deps.Website as Website
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified File
import qualified Http
import qualified Json.Decode as D
import qualified Parse.Primitives as P
import qualified Reporting.Exit as Exit
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
  File.readBinary (Stuff.registry cache)



-- FETCH


fetch :: Http.Manager -> Stuff.PackageCache -> IO (Either Exit.RegistryProblem Registry)
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
  let
    keyDecoder =
      Pkg.keyDecoder bail

    versionsDecoder =
      D.list (D.mapError (\_ -> ()) V.decoder)

    toKnownVersions versions =
      case List.sortBy (flip compare) versions of
        v:vs -> return (KnownVersions v vs)
        []   -> D.failure ()
  in
  D.dict keyDecoder (toKnownVersions =<< versionsDecoder)



-- UPDATE


update :: Http.Manager -> Stuff.PackageCache -> Registry -> IO (Either Exit.RegistryProblem Registry)
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



-- NEW PACKAGE DECODER


newPkgDecoder :: D.Decoder () (Pkg.Name, V.Version)
newPkgDecoder =
  D.customString newPkgParser bail


newPkgParser :: P.Parser () (Pkg.Name, V.Version)
newPkgParser =
  do  pkg <- P.specialize (\_ _ _ -> ()) Pkg.parser
      P.word1 0x40 {-@-} bail
      vsn <- P.specialize (\_ _ _ -> ()) V.parser
      return (pkg, vsn)


bail :: row -> col -> ()
bail _ _ =
  ()



-- LATEST


latest :: Http.Manager -> Stuff.PackageCache -> IO (Either Exit.RegistryProblem Registry)
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
    Nothing -> Left $ Pkg.nearbyNames name (Map.keys versions)



-- POST


post :: Http.Manager -> String -> D.Decoder x a -> (a -> IO b) -> IO (Either Exit.RegistryProblem b)
post manager path decoder callback =
  let
    url = Website.route path []
  in
  Http.post manager url [] Exit.RP_Http $
    \body ->
      case D.fromByteString decoder body of
        Right a -> Right <$> callback a
        Left _ -> return $ Left $ Exit.RP_Data url body



-- BINARY


instance Binary Registry where
  get = liftM2 Registry get get
  put (Registry a b) = put a >> put b


instance Binary KnownVersions where
  get = liftM2 KnownVersions get get
  put (KnownVersions a b) = put a >> put b
