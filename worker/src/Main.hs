{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main
  ( main
  )
  where


import Control.Applicative ((<|>))
import Control.Concurrent (readMVar)
import Control.Monad (liftM2, msum)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import qualified Data.OneOrMore as OneOrMore
import Network.URI (parseURI)
import Snap.Core
import Snap.Http.Server
import Snap.Util.CORS
import Snap.Util.FileServe (serveFile)
import Snap.Util.FileUploads
import qualified System.Directory as Dir
import qualified System.IO.Streams as Stream
import Text.RawString.QQ (r)

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified Compile
import qualified Elm.Details as Details
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Generate.Html as Html
import qualified Generate.JavaScript as Generate
import qualified Generate.Mode as Mode
import qualified Json.Encode as Encode
import qualified Parse.Module as Parse
import qualified Reporting
import qualified Reporting.Error as Error
import qualified Reporting.Render.Code as Code



-- RUN THE DEV SERVER


main :: IO ()
main =
  do  artifacts <- loadArtifacts
      httpServe config $ msum $
        [ path "compile" $ compile artifacts
        , path "compile/error.js" $ serveFile "error.js"
        , notFound
        ]


config :: Config Snap a
config =
  defaultConfig
    # setVerbose False
    # setPort 8000
    # setAccessLog ConfigNoLog
    # setErrorLog ConfigNoLog


(#) :: a -> (a -> b) -> b
(#) value func =
  func value



-- NOT FOUND


notFound :: Snap ()
notFound =
  do  modifyResponse $ setResponseStatus 404 "Not Found"
      modifyResponse $ setContentType "text/html; charset=utf-8"
      writeBS "Not Found"



-- COMPILE


compile :: Artifacts -> Snap ()
compile (Artifacts ifaces objs) =
  applyCORS corsOptions $ method POST $
    do  parts <- handleMultipart defaultUploadPolicy handlePart
        case parts of
          [Just source] ->
            case compileByteString ifaces source of
              Left err ->
                writeBuilder $ errorToHtmlBuilder source err

              Right (home, locals, main) ->
                let
                  mode  = Mode.Dev Nothing
                  name  = ModuleName._module home
                  mains = Map.singleton home main
                  graph = Opt.addLocalGraph locals objs
                in
                writeBuilder $ Html.sandwich name $ Generate.generate mode graph mains

          _ ->
            pass


handlePart :: PartInfo -> Stream.InputStream B.ByteString -> IO (Maybe B.ByteString)
handlePart info stream =
  if partFieldName info == "code" && partDisposition info == DispositionFormData
  then Just . LBS.toStrict <$> storeAsLazyByteString stream
  else return Nothing


compileByteString :: Map.Map ModuleName.Raw I.Interface -> B.ByteString -> Either Error.Error (ModuleName.Canonical, Opt.LocalGraph, Opt.Main)
compileByteString ifaces source =
  case Parse.fromByteString Pkg.dummyName source of
    Left err ->
      Left (Error.BadSyntax err)

    Right modul ->
      case Compile.compile Pkg.dummyName ifaces modul of
        Left err ->
          Left err

        Right artifacts@(Compile.Artifacts modul _ locals@(Opt.LocalGraph maybeMain _ _)) ->
          case maybeMain of
            Just main -> Right (Can._name modul, locals, main)
            Nothing   -> Left (error "TODO no main")


errorToHtmlBuilder :: B.ByteString -> Error.Error -> B.Builder
errorToHtmlBuilder source err =
  let
    json = Encode.encodeUgly (Error.errorToJson (Code.toSource source) err)
  in
  [r|<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <style>body { padding: 0; margin: 0; }</style>
  <script src="https://worker.elm-lang.org/compile/error.js"></script>
  <script>Elm.Error.init({flags:|] <> json <> [r|})</script>
</head>
<body></body>
</html>|]



-- ARTIFACTS


data Artifacts =
  Artifacts
    { _ifaces :: Map.Map ModuleName.Raw I.Interface
    , _graph :: Opt.GlobalGraph
    }


loadArtifacts :: IO Artifacts
loadArtifacts =
  do  style <- Reporting.terminal
      root <- Dir.getCurrentDirectory
      result <- Details.load style root
      case result of
        Left _ ->
          error "Ran into some problem loading elm.json details"

        Right details ->
          do  omvar <- Details.loadObjects root details
              imvar <- Details.loadInterfaces root details
              mdeps <- readMVar imvar
              mobjs <- readMVar omvar
              case liftM2 (,) mdeps mobjs of
                Nothing ->
                  error "Ran into some weird problem loading elm.json details"

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



-- CORS


corsOptions :: (Monad m) => CORSOptions m
corsOptions =
  CORSOptions
    { corsAllowOrigin = return allowedOrigins
    , corsAllowCredentials = return True
    , corsExposeHeaders = return HashSet.empty
    , corsAllowedMethods = return allowedMethods
    , corsAllowedHeaders = return
    }


{-# NOINLINE allowedMethods #-}
allowedMethods :: HashSet.HashSet HashableMethod
allowedMethods =
  HashSet.singleton (HashableMethod POST)


{-# NOINLINE allowedOrigins #-}
allowedOrigins :: OriginList
allowedOrigins =
  let
    origins =
      [ "https://elm-lang.org"
      , "https://package.elm-lang.org"
      ]
  in
  Origins $ mkOriginSet $
    case traverse parseURI origins of
      Just uris -> uris
      Nothing -> error "Invalid entry in allowedOrigins list"
