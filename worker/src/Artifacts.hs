{-# OPTIONS_GHC -Wall #-}
module Artifacts
  ( Artifacts(..)
  , loadCompile
  , loadRepl
  , toDepsInfo
  )
  where


import Control.Concurrent (readMVar)
import Control.Monad (liftM2)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Name as N
import qualified Data.OneOrMore as OneOrMore
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified BackgroundWriter as BW
import qualified Elm.Details as Details
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import Json.Encode ((==>))
import qualified Json.Encode as E
import qualified Json.String as Json
import qualified Reporting



-- ARTIFACTS


data Artifacts =
  Artifacts
    { _ifaces :: Map.Map ModuleName.Raw I.Interface
    , _graph :: Opt.GlobalGraph
    }


loadCompile :: IO Artifacts
loadCompile =
  load ("outlines" </> "compile")


loadRepl :: IO Artifacts
loadRepl =
  load ("outlines" </> "repl")



-- LOAD


load :: FilePath -> IO Artifacts
load dir =
  BW.withScope $ \scope ->
  do  putStrLn $ "Loading " ++ dir </> "elm.json"
      style <- Reporting.terminal
      root <- fmap (</> dir) Dir.getCurrentDirectory
      result <- Details.load style scope root
      case result of
        Left _ ->
          error $ "Ran into some problem loading elm.json\nTry running `elm make` in: " ++ dir

        Right details ->
          do  omvar <- Details.loadObjects root details
              imvar <- Details.loadInterfaces root details
              mdeps <- readMVar imvar
              mobjs <- readMVar omvar
              case liftM2 (,) mdeps mobjs of
                Nothing ->
                  error $ "Ran into some weird problem loading elm.json\nTry running `elm make` in: " ++ dir

                Just (deps, objs) ->
                  return $ Artifacts (toInterfaces deps) objs


toInterfaces :: Map.Map ModuleName.Canonical I.DependencyInterface -> Map.Map ModuleName.Raw I.Interface
toInterfaces deps =
  Map.mapMaybe toUnique $ Map.fromListWith OneOrMore.more $
    Map.elems (Map.mapMaybeWithKey getPublic deps)


getPublic :: ModuleName.Canonical -> I.DependencyInterface -> Maybe (ModuleName.Raw, OneOrMore.OneOrMore I.Interface)
getPublic (ModuleName.Canonical _ name) dep =
  case dep of
    I.Public  iface -> Just (name, OneOrMore.one iface)
    I.Private _ _ _ -> Nothing


toUnique :: OneOrMore.OneOrMore a -> Maybe a
toUnique oneOrMore =
  case oneOrMore of
    OneOrMore.One value -> Just value
    OneOrMore.More _ _  -> Nothing



-- TO DEPS INFO


toDepsInfo :: Artifacts -> BS.ByteString
toDepsInfo (Artifacts ifaces _) =
  LBS.toStrict $ B.toLazyByteString $ E.encodeUgly $ encode ifaces



-- ENCODE


encode :: Map.Map ModuleName.Raw I.Interface -> E.Value
encode ifaces =
  E.dict Json.fromName encodeInterface ifaces


encodeInterface :: I.Interface -> E.Value
encodeInterface (I.Interface pkg values unions aliases binops) =
  E.object
    [ "pkg" ==> E.chars (Pkg.toChars pkg)
    , "ops" ==> E.list E.name (Map.keys binops)
    , "values" ==> E.list E.name (Map.keys values)
    , "aliases" ==> E.list E.name (Map.keys (Map.filter isPublicAlias aliases))
    , "types" ==> E.dict Json.fromName (E.list E.name) (Map.mapMaybe toPublicUnion unions)
    ]


isPublicAlias :: I.Alias -> Bool
isPublicAlias alias =
  case alias of
    I.PublicAlias  _ -> True
    I.PrivateAlias _ -> False


toPublicUnion :: I.Union -> Maybe [N.Name]
toPublicUnion union =
  case union of
    I.OpenUnion (Can.Union _ variants _ _) ->
      Just (map getVariantName variants)

    I.ClosedUnion _ ->
      Just []

    I.PrivateUnion _ ->
      Nothing


getVariantName :: Can.Ctor -> N.Name
getVariantName (Can.Ctor name _ _ _) =
  name
