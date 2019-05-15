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
import qualified Data.Map.Utils as Map
import qualified Data.NonEmptyList as NE
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
import qualified Reporting.Annotation as A
import qualified Reporting.Error as Error
import qualified Reporting.Error.Import as Import
import qualified Reporting.Render.Code as Code



-- RUN THE DEV SERVER


main :: IO ()
main =
  do  artifacts <- loadArtifacts
      errorHtml <- compileErrorViewer artifacts
      httpServe config $ msum $
        [ path "compile" $ compile artifacts
        , path "compile/error.js" $ writeBS errorHtml
        , notFound
        ]


config :: Config Snap a
config =
  defaultConfig
    |> setVerbose False
    |> setPort 8000
    |> setAccessLog ConfigNoLog
    |> setErrorLog ConfigNoLog


(|>) :: a -> (a -> b) -> b
(|>) value func =
  func value



-- NOT FOUND


notFound :: Snap ()
notFound =
  do  modifyResponse $ setResponseStatus 404 "Not Found"
      modifyResponse $ setContentType "text/html; charset=utf-8"
      writeBS "Not Found"



-- COMPILE


compile :: Artifacts -> Snap ()
compile artifacts =
  applyCORS corsOptions $ method POST $
    do  parts <- handleMultipart defaultUploadPolicy handlePart
        case parts of
          [Just source] ->
            case compileToBuilder artifacts source of
              Left err ->
                writeBuilder $ errorToHtmlBuilder source err

              Right builder ->
                writeBuilder builder

          _ ->
            pass


handlePart :: PartInfo -> Stream.InputStream B.ByteString -> IO (Maybe B.ByteString)
handlePart info stream =
  if partFieldName info == "code" && partDisposition info == DispositionFormData
  then Just . LBS.toStrict <$> storeAsLazyByteString stream
  else return Nothing


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



compileToBuilder :: Artifacts -> B.ByteString -> Either Error.Error B.Builder
compileToBuilder (Artifacts interfaces objects) source =
  case Parse.fromByteString Pkg.dummyName source of
    Left err ->
      Left (Error.BadSyntax err)

    Right modul@(Src.Module _ _ _ imports _ _ _ _ _) ->
      case checkImports interfaces imports of
        Left err ->
          Left err

        Right ifaces ->
          case Compile.compile Pkg.dummyName ifaces modul of
            Left err ->
              Left err

            Right artifacts@(Compile.Artifacts modul _ locals@(Opt.LocalGraph maybeMain _ _)) ->
              case maybeMain of
                Nothing ->
                  Left (error "TODO no main")

                Just main ->
                  let
                    mode  = Mode.Dev Nothing
                    home  = Can._name modul
                    name  = ModuleName._module home
                    mains = Map.singleton home main
                    graph = Opt.addLocalGraph locals objects
                  in
                  Right $ Html.sandwich name $ Generate.generate mode graph mains


checkImports :: Map.Map ModuleName.Raw I.Interface -> [Src.Import] -> Either Error.Error (Map.Map ModuleName.Raw I.Interface)
checkImports interfaces imports =
  let
    importDict = Map.fromValues Src.getImportName imports
    missing = Map.difference importDict interfaces
  in
  case Map.elems missing of
    [] ->
      Right (Map.intersection interfaces importDict)

    i:is ->
      let
        unimported = Map.keysSet (Map.difference interfaces importDict)
        toError (Src.Import (A.At region name) _ _) =
          Import.Error region name unimported Import.NotFound
      in
      Left (Error.BadImports (fmap toError (NE.List i is)))



-- COMPILE ERROR VIEWER


compileErrorViewer :: Artifacts -> B.ByteString
compileErrorViewer artifacts =
  do  source <- File.readUtf8 "src/Error.elm"
      case compileToBuilder artifacts source of
        Left err ->
          error "problem compiling src/Error.elm"

        Right builder ->
          return (LBS.toStrict (B.toLazyByteString builder))



-- CORS OPTIONS


corsOptions :: (Monad m) => CORSOptions m
corsOptions =
  let
    allowedOrigins = toOriginList [ "https://elm-lang.org", "https://package.elm-lang.org" ]
    allowedMethods = HashSet.singleton (HashableMethod POST)
  in
  CORSOptions
    { corsAllowOrigin = return allowedOrigins
    , corsAllowCredentials = return True
    , corsExposeHeaders = return HashSet.empty
    , corsAllowedMethods = return allowedMethods
    , corsAllowedHeaders = return
    }


toOriginList :: [String] -> OriginList
toOriginList origins =
  Origins $ mkOriginSet $
    case traverse parseURI origins of
      Just uris -> uris
      Nothing -> error "invalid entry given to toOriginList list"



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
