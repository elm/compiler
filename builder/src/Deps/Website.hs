{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Deps.Website
  ( getElmJson
  , getDocs
  , getNewPackages
  , getAllPackages
  , download
  , githubCommit
  , githubDownload
  , Sha
  , register
  )
  where


import Prelude hiding (zip)
import qualified Codec.Archive.Zip as Zip
import Control.Monad (void)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.MultipartFormData as Multi
import qualified Network.HTTP.Types as Http
import qualified System.Directory as Dir
import System.FilePath ((</>), splitFileName)

import Elm.Package (Name, Version)
import qualified Elm.Package as Pkg
import qualified Json.Decode as D

import qualified Reporting.Doc as D
import qualified Reporting.Exit.Http as E
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task
import qualified Reporting.Task.Http as Http
import qualified Stuff.Paths as Path



-- GET PACKAGE INFO


getElmJson :: Name -> Version -> Task.Task BS.ByteString
getElmJson name version =
  Http.run $ fetchByteString $
    "packages/" ++ Pkg.toUrl name ++ "/" ++ Pkg.versionToString version ++ "/elm.json"


getDocs :: Name -> Version -> Task.Task BS.ByteString
getDocs name version =
  Http.run $ fetchByteString $
    "packages/" ++ Pkg.toUrl name ++ "/" ++ Pkg.versionToString version ++ "/docs.json"



-- NEW PACKAGES


getNewPackages :: Int -> Task.Task [(Name, Version)]
getNewPackages index =
  Http.run $ fetchJson "packages" E.badJsonToDocs (D.list newPkgDecoder) ("all-packages/since/" ++ show index)


newPkgDecoder :: D.Decoder E.BadJson ( Name, Version )
newPkgDecoder =
  do  txt <- D.text
      case Text.splitOn "@" txt of
        [key, value] ->
          case Pkg.fromText key of
            Right pkg ->
              case Pkg.versionFromText value of
                Just vsn ->
                  D.succeed (pkg, vsn)

                Nothing ->
                  D.fail (E.BadNewPkg txt)

            Left _ ->
              D.fail (E.BadNewPkg txt)

        _ ->
          D.fail (E.BadNewPkg txt)



-- ALL PACKAGES


getAllPackages :: Task.Task (Map.Map Name [Version])
getAllPackages =
  Http.run $ fetchJson "packages" E.badJsonToDocs allPkgsDecoder "all-packages"


allPkgsDecoder :: D.Decoder E.BadJson (Map.Map Name [Version])
allPkgsDecoder =
  let
    checkKeys pairs pkgs =
      case pairs of
        [] ->
          D.succeed pkgs

        (key, versions) : rest ->
          case Pkg.fromText key of
            Left (msg, _) ->
              D.fail (E.BadAllPkg key msg)

            Right pkg ->
              checkKeys rest (Map.insert pkg versions pkgs)
  in
    do  dict <- D.dict (D.list (D.mapError E.BadAllVsn Pkg.versionDecoder))
        checkKeys (HashMap.toList dict) Map.empty



-- HELPERS


fetchByteString :: String -> Http.Fetch BS.ByteString
fetchByteString path =
  Http.package path [] $ \request manager ->
    do  response <- Client.httpLbs request manager
        return $ Right $ LBS.toStrict $ Client.responseBody response


fetchJson :: String -> (e -> [D.Doc]) -> D.Decoder e a -> String -> Http.Fetch a
fetchJson rootName errorToDocs decoder path =
  Http.package path [] $ \request manager ->
    do  response <- Client.httpLbs request manager
        let bytes = LBS.toStrict (Client.responseBody response)
        case D.parse rootName errorToDocs decoder bytes of
          Right value ->
            return $ Right value

          Left jsonProblem ->
            return $ Left $ E.BadJson path jsonProblem



-- DOWNLOAD


download :: [(Name, Version)] -> Task.Task ()
download packages =
  case packages of
    [] ->
      Task.report Progress.DownloadSkip

    _ ->
      do  cache <- Task.getPackageCacheDir

          let start = Progress.DownloadStart packages
          let toEnd = Progress.DownloadEnd

          void $ Http.run $ Http.report start toEnd $
            Http.parallel $ map (downloadHelp cache) packages


downloadHelp :: FilePath -> (Name, Version) -> Http.Fetch ()
downloadHelp cache (name, version) =
  let
    endpointUrl =
      "packages/" ++ Pkg.toUrl name ++ "/" ++ Pkg.versionToString version ++ "/endpoint.json"
  in
    Http.andThen (fetchJson "version" id endpointDecoder endpointUrl) $ \(endpoint, hash) ->
      let
        start = Progress.DownloadPkgStart name version
        toEnd = Progress.DownloadPkgEnd name version
      in
        Http.report start toEnd $
          Http.anything endpoint $ \request manager ->
            Client.withResponse request manager (downloadArchive cache name version hash)


endpointDecoder :: D.Decoder e (String, String)
endpointDecoder =
  D.map2 (,)
    (D.field "url" D.string)
    (D.field "hash" D.string)



-- DOWNLOAD ZIP ARCHIVE


downloadArchive :: FilePath -> Name -> Version -> String -> Client.Response Client.BodyReader -> IO (Either E.Exit ())
downloadArchive cache name version expectedHash response =
  do  result <- readArchive (Client.responseBody response) initialArchiveState
      case result of
        Left msg ->
          return (Left msg)

        Right (sha, archive) ->
          if expectedHash == SHA.showDigest sha then
            Right <$> writeArchive archive (cache </> Pkg.toFilePath name) (Pkg.versionToString version)
          else
            return $ Left $ E.BadZipSha expectedHash (SHA.showDigest sha)



-- READ ARCHIVE


data ArchiveState =
  AS
    { _len :: !Int
    , _sha :: !(Binary.Decoder SHA.SHA1State)
    , _zip :: !(Binary.Decoder Zip.Archive)
    }


initialArchiveState :: ArchiveState
initialArchiveState =
  AS 0 SHA.sha1Incremental (Binary.runGetIncremental Binary.get)


type Sha = SHA.Digest SHA.SHA1State


readArchive :: Client.BodyReader -> ArchiveState -> IO (Either E.Exit (Sha, Zip.Archive))
readArchive body (AS len sha zip) =
  case zip of
    Binary.Fail _ _ _ ->
      return $ Left E.BadZipData

    Binary.Partial k ->
      do  chunk <- Client.brRead body
          readArchive body $ AS
            { _len = len + BS.length chunk
            , _sha = Binary.pushChunk sha chunk
            , _zip = k (if BS.null chunk then Nothing else Just chunk)
            }

    Binary.Done _ _ archive ->
      return $ Right ( SHA.completeSha1Incremental sha len, archive )



-- WRITE ARCHIVE


writeArchive :: Zip.Archive -> FilePath -> FilePath -> IO ()
writeArchive archive destination newRoot =
  do  Dir.createDirectoryIfMissing True destination
      let opts = [Zip.OptDestination destination]
      mapM_ (Zip.writeEntry opts . replaceRoot newRoot) (Zip.zEntries archive)


replaceRoot :: String -> Zip.Entry -> Zip.Entry
replaceRoot root entry =
  let
    rootless =
      dropWhile (/='/') (Zip.eRelativePath entry)
  in
    entry { Zip.eRelativePath = root ++ rootless }



-- FIND TAGGED COMMIT ON GITHUB


githubCommit :: Name -> Version -> Task.Task String
githubCommit name version =
  let
    endpoint =
      "https://api.github.com/repos/" ++ Pkg.toUrl name ++ "/git/refs/tags/" ++ Pkg.versionToString version

    headers =
      [ ( Http.hUserAgent, "elm-cli" )
      , ( Http.hAccept, "application/json" )
      ]

    decoder =
      D.at ["object","sha"] D.string
  in
    Http.run $ Http.anything endpoint $ \request manager ->
      do  response <- Client.httpLbs (request { Client.requestHeaders = headers }) manager
          let bytes = LBS.toStrict (Client.responseBody response)
          case D.parse "github" id decoder bytes of
            Right value ->
              return $ Right value

            Left jsonProblem ->
              return $ Left $ E.BadJson "github.json" jsonProblem



-- DOWNLOAD FROM GITHUB


githubDownload :: Name -> Version -> FilePath -> Task.Task Sha
githubDownload name version dir =
  let
    endpoint =
      "https://github.com/" ++ Pkg.toUrl name ++ "/zipball/" ++ Pkg.versionToString version ++ "/"
  in
    Http.run $ Http.anything endpoint $ \request manager ->
      Client.withResponse request manager (githubDownloadHelp dir)


githubDownloadHelp :: FilePath -> Client.Response Client.BodyReader -> IO (Either E.Exit Sha)
githubDownloadHelp targetDir response =
  do  result <- readArchive (Client.responseBody response) initialArchiveState
      case result of
        Left msg ->
          return (Left msg)

        Right (sha, archive) ->
          do  let (dir, root) = splitFileName targetDir
              writeArchive archive dir root
              return $ Right sha



-- REGISTER PACKAGES


register :: Name -> Version -> String -> Sha -> Task.Task ()
register name version commitHash digest =
  let
    params =
      [ ("name", Pkg.toString name)
      , ("version", Pkg.versionToString version)
      , ("commit-hash", commitHash)
      ]

    files =
      [ Multi.partFileSource "elm.json" "elm.json"
      , Multi.partFileSource "docs.json" Path.docs
      , Multi.partFileSource "README.md" "README.md"
      , Multi.partFileRequestBody "github-hash" "github-hash" $
          Client.RequestBodyBS (BS.pack (SHA.showDigest digest))
      ]
  in
    Http.run $ Http.package "register" params $ \rawRequest manager ->
      do  requestWithBody <- Multi.formDataBody files rawRequest
          let request = requestWithBody { Client.responseTimeout = Client.responseTimeoutNone }
          void $ Client.httpLbs request manager
          return $ Right ()
