{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main
  ( main
  )
  where


import Control.Concurrent (readMVar)
import Control.Monad (liftM2, msum)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import qualified Data.Map.Utils as Map
import qualified Data.Name as N
import qualified Data.NonEmptyList as NE
import qualified Data.OneOrMore as OneOrMore
import Network.URI (parseURI)
import Snap.Core
import Snap.Http.Server
import Snap.Util.CORS (CORSOptions(..), HashableMethod(..), OriginList(Origins), applyCORS, mkOriginSet)
import Snap.Util.FileUploads
import qualified System.Directory as Dir
import qualified System.IO.Streams as Stream
import Text.RawString.QQ (r)

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified BackgroundWriter as BW
import qualified Build
import qualified Compile
import qualified Elm.Details as Details
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified File
import qualified Generate
import qualified Generate.Html as Html
import qualified Generate.JavaScript as Generate
import qualified Generate.Mode as Mode
import qualified Json.Encode as Encode
import qualified Parse.Module as Parse
import qualified Reporting
import qualified Reporting.Annotation as A
import qualified Reporting.Error as Error
import qualified Reporting.Error.Import as Import
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task



-- RUN THE DEV SERVER


main :: IO ()
main =
  do  artifacts <- loadArtifacts
      errorJS <- compileErrorViewer
      httpServe config $ msum $
        [ path "compile" $ compile artifacts
        , path "compile/errors.js" $ writeBS errorJS
        , notFound
        ]


config :: Config Snap a
config =
  defaultConfig
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



-- COMPILE ENDPOINT


compile :: Artifacts -> Snap ()
compile artifacts =
  applyCORS corsOptions $ method POST $
    do  result <- foldMultipart defaultUploadPolicy ignoreFile 0
        case result of
          ([("code",source)], 0) ->
            do  modifyResponse $ setContentType "text/html; charset=utf-8"
                case compileToBuilder artifacts source of
                  Right builder -> writeBuilder builder
                  Left exit     -> writeBuilder (exitToHtmlBuilder exit)

          _ ->
            do  modifyResponse $ setResponseStatus 400 "Bad Request"
                modifyResponse $ setContentType "text/html; charset=utf-8"
                writeBS
                  "<p>Unexpected request format. This should not be possible!</p>\
                  \<p>Please report this\
                  \ <a href=\"https://github.com/elm/compiler/issues\">here</a>\
                  \ along with the URL and your browser version.</p>"


ignoreFile :: PartInfo -> Stream.InputStream B.ByteString -> Int -> IO Int
ignoreFile _ _ count =
  return (count + 1)


exitToHtmlBuilder :: Exit.Worker -> B.Builder
exitToHtmlBuilder exit =
  let
    json =
      Encode.encodeUgly $ Exit.toJson $ Exit.workerToReport exit
  in
  [r|<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <style>body { padding: 0; margin: 0; background-color: black; }</style>
  <script src="https://worker.elm-lang.org/compile/errors.js"></script>
</head>
<body>
  <script>Elm.Errors.init({flags:|] <> json <> [r|});</script>
</body>
</html>|]



-- COMPILE TO BUILDER


compileToBuilder :: Artifacts -> B.ByteString -> Either Exit.Worker B.Builder
compileToBuilder (Artifacts interfaces objects) source =
  case Parse.fromByteString Pkg.dummyName source of
    Left err ->
      Left $ toInputError N._Main source (Error.BadSyntax err)

    Right modul@(Src.Module _ _ _ imports _ _ _ _ _) ->
      case checkImports interfaces imports of
        Left err ->
          Left $ toInputError (Src.getName modul) source (Error.BadImports err)

        Right ifaces ->
          case Compile.compile Pkg.dummyName ifaces modul of
            Left err ->
              Left $ toInputError (Src.getName modul) source err

            Right (Compile.Artifacts canModule _ locals) ->
              case locals of
                Opt.LocalGraph Nothing _ _ ->
                  Left Exit.WorkerNoMain

                Opt.LocalGraph (Just main_) _ _ ->
                  let
                    mode  = Mode.Dev Nothing
                    home  = Can._name canModule
                    name  = ModuleName._module home
                    mains = Map.singleton home main_
                    graph = Opt.addLocalGraph locals objects
                  in
                  Right $ Html.sandwich name $ Generate.generate mode graph mains


checkImports :: Map.Map ModuleName.Raw I.Interface -> [Src.Import] -> Either (NE.List Import.Error) (Map.Map ModuleName.Raw I.Interface)
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
      Left (fmap toError (NE.List i is))


toInputError :: ModuleName.Raw -> B.ByteString -> Error.Error -> Exit.Worker
toInputError name source err =
  Exit.WorkerInputError $
    Error.Module name "/try" File.zeroTime source err



-- COMPILE ERROR VIEWER


compileErrorViewer :: IO B.ByteString
compileErrorViewer =
  let
    run work =
      do  result <- work
          case result of
            Right a -> return a
            Left _ -> error "problem building reactor/src/Errors.elm"
  in
  Dir.withCurrentDirectory "../reactor" $ BW.withScope $ \scope ->
    do  let root = "."
        details <- run $ Details.load Reporting.silent scope root
        artifacts <- run $ Build.fromMains Reporting.silent root details (NE.List "src/Errors.elm" [])
        javascript <- run $ Task.run $ Generate.prod root details artifacts
        return $ LBS.toStrict $ B.toLazyByteString javascript



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
  BW.withScope $ \scope ->
  do  style <- Reporting.terminal
      root <- Dir.getCurrentDirectory
      result <- Details.load style scope root
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
