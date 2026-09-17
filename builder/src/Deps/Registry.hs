{-# LANGUAGE BangPatterns, ExtendedLiterals, OverloadedStrings #-}
module Deps.Registry
  ( Registry(..)
  , KnownVersions(..)
  , read
  , fetch
  , update
  , latest
  , getVersions
  , getVersions'
  --
  , eRegistry, dRegistry
  )
  where


import Prelude hiding (read)
import Control.Monad (liftM2)
import qualified Data.List as List
import qualified Data.Map.Strict as Map

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E

import qualified Deps.Website as Website
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified File
import qualified Http
import qualified Json.Decode as JD
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
  File.readBytes dRegistry (Stuff.registry cache)



-- FETCH


fetch :: File.Writer Stuff.PACKAGES -> Http.Manager -> Stuff.PackageCache -> IO (Either Exit.RegistryProblem Registry)
fetch writer manager cache =
  post manager "/all-packages" allPkgsDecoder $
    \versions ->
      do  let size = Map.foldr' addEntry 0 versions
          let registry = Registry size versions
          let path = Stuff.registry cache
          File.writeBytes writer path eRegistry registry
          return registry


addEntry :: KnownVersions -> Int -> Int
addEntry (KnownVersions _ vs) count =
  count + 1 + length vs


allPkgsDecoder :: JD.Decoder () (Map.Map Pkg.Name KnownVersions)
allPkgsDecoder =
  let
    keyDecoder =
      Pkg.keyDecoder bail

    versionsDecoder =
      JD.list (JD.mapError (\_ -> ()) V.decoder)

    toKnownVersions versions =
      case List.sortBy (flip compare) versions of
        v:vs -> return (KnownVersions v vs)
        []   -> JD.failure ()
  in
  JD.dict keyDecoder (toKnownVersions =<< versionsDecoder)



-- UPDATE


update :: File.Writer Stuff.PACKAGES -> Http.Manager -> Stuff.PackageCache -> Registry -> IO (Either Exit.RegistryProblem Registry)
update writer manager cache oldRegistry@(Registry size packages) =
  post manager ("/all-packages/since/" ++ show size) (JD.list newPkgDecoder) $
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
          do  File.writeBytes writer (Stuff.registry cache) eRegistry newRegistry
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


newPkgDecoder :: JD.Decoder () (Pkg.Name, V.Version)
newPkgDecoder =
  JD.customString newPkgParser bail


newPkgParser :: P.Parser () (Pkg.Name, V.Version)
newPkgParser =
  do  pkg <- P.specialize (\_ _ -> ()) Pkg.parser
      P.word1 0x40#Word8 {-@-} bail
      vsn <- P.specialize (\_ _ -> ()) V.parser
      return (pkg, vsn)


bail :: P.Cursor -> ()
bail _ =
  ()



-- LATEST


latest :: File.Writer Stuff.PACKAGES -> Http.Manager -> Stuff.PackageCache -> IO (Either Exit.RegistryProblem Registry)
latest writer manager cache =
  do  maybeOldRegistry <- read cache
      case maybeOldRegistry of
        Just oldRegistry ->
          update writer manager cache oldRegistry

        Nothing ->
          fetch writer manager cache



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


post :: Http.Manager -> String -> JD.Decoder x a -> (a -> IO b) -> IO (Either Exit.RegistryProblem b)
post manager path decoder callback =
  let
    url = Website.route path []
  in
  Http.post manager url [] Exit.RP_Http $
    \body ->
      do  result <- JD.fromByteString decoder body
          case result of
            Right a -> Right <$> callback a
            Left _ -> return $ Left $ Exit.RP_Data url body



-- BINARY


eRegistry :: Registry -> E.Builder
eRegistry (Registry c vs) =
  E.int c <> E.dict32 Pkg.eName eKnownVersions vs


dRegistry :: D.Decoder Registry
dRegistry =
  liftM2 Registry D.int (D.dict32 Pkg.dName dKnownVersions)


eKnownVersions :: KnownVersions -> E.Builder
eKnownVersions (KnownVersions n p) =
  V.eVersion n <> E.list32 V.eVersion p


dKnownVersions :: D.Decoder KnownVersions
dKnownVersions =
  liftM2 KnownVersions V.dVersion (D.list32 V.dVersion)

