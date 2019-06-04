{-# LANGUAGE OverloadedStrings #-}
module Publish
  ( run
  )
  where


import Control.Exception (bracket_)
import Control.Monad (void)
import qualified Data.List as List
import qualified Data.Utf8 as Utf8
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import System.FilePath ((</>))
import qualified System.Info as Info
import qualified System.IO as IO
import qualified System.Process as Process

import qualified Deps.Bump as Bump
import qualified Deps.Diff as Diff
import qualified Deps.Registry as Registry
import qualified Deps.Website as Website
import qualified Elm.Docs as Docs
import qualified Elm.Magnitude as M
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified File
import qualified Http
import qualified Json.Decode as D
import qualified Json.String as Json
import qualified Reporting
import Reporting.Doc ((<+>))
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Task as Task
import qualified Stuff



-- RUN


-- TODO mandate no "exposing (..)" in packages to make
-- optimization to skip builds in Elm.Details always valid


run :: () -> () -> IO ()
run () () =
  Reporting.attempt Exit.publishToReport $
    Task.run $ publish =<< getEnv



-- ENV


data Env =
  Env
    { _root :: FilePath
    , _cache :: Stuff.PackageCache
    , _manager :: Http.Manager
    , _registry :: Registry.Registry
    , _outline :: Outline.Outline
    }


getEnv :: Task.Task Exit.Publish Env
getEnv =
  do  maybeRoot <- Task.io $ Stuff.findRoot
      case maybeRoot of
        Nothing ->
          Task.throw Exit.PublishNoOutline

        Just root ->
          do  cache <- Task.io $ Stuff.getPackageCache
              manager <- Task.io $ Http.getManager
              registry <- Task.eio Exit.PublishMustHaveLatestRegistry $ Registry.latest manager cache
              outlineResult <- Task.io $ Outline.read root
              case outlineResult of
                Right outline ->
                  return $ Env root cache manager registry outline

                Left problem ->
                  Task.throw $ Exit.PublishBadOutline problem



-- PUBLISH


publish ::  Env -> Task.Task Exit.Publish ()
publish env@(Env root cache manager registry outline) =
  case outline of
    Outline.App _ ->
      Task.throw Exit.PublishApplication

    Outline.Pkg (Outline.PkgOutline pkg summary _ vsn exposed _ _ _) ->
      do  let maybeKnownVersions = Registry.getVersions pkg registry

          reportPublishStart pkg vsn maybeKnownVersions

          if noExposed  exposed then Task.throw Exit.PublishNoExposed else return ()
          if badSummary summary then Task.throw Exit.PublishNoSummary else return ()

          verifyReadme root
          verifyLicense root
          docs <- error "TODO Project.generateDocs details" details
          verifyVersion env pkg vsn docs maybeKnownVersions
          commitHash <- verifyTag manager pkg vsn
          verifyNoChanges commitHash vsn
          zipHash <- verifyZip manager pkg vsn

          register manager pkg vsn commitHash zipHash

          putStrLn "Success!"



-- VERIFY SUMMARY


badSummary :: Json.String -> Bool
badSummary summary =
  Json.isEmpty summary || Outline.defaultSummary == summary


noExposed :: Outline.Exposed -> Bool
noExposed exposed =
  case exposed of
    Outline.ExposedList modules ->
      null modules

    Outline.ExposedDict chunks ->
      all (null . snd) chunks



-- VERIFY README


verifyReadme :: FilePath -> Task.Task Exit.Publish ()
verifyReadme root =
  reportReadmePhase $
  do  let readmePath = root </> "README.md"
      exists <- File.exists readmePath
      case exists of
        False ->
          return (Left Exit.PublishNoReadme)

        True ->
          do  size <- IO.withFile readmePath IO.ReadMode IO.hFileSize
              if size < 300
                then return (Left Exit.PublishShortReadme)
                else return (Right ())



-- VERIFY LICENSE


verifyLicense :: FilePath -> Task.Task Exit.Publish ()
verifyLicense root =
  reportLicensePhase $
  do  let licensePath = root </> "LICENSE"
      exists <- File.exists licensePath
      if exists
        then return (Right ())
        else return (Left Exit.PublishNoLicense)



-- VERIFY GITHUB TAG


verifyTag :: Http.Manager -> Pkg.Name -> V.Version -> Task.Task Exit.Publish String
verifyTag manager pkg vsn =
  reportTagPhase vsn $
  Http.post manager (toTagUrl pkg vsn) [Http.accept "application/json"] Exit.PublishCannotGetTag $ \body ->
    case D.fromByteString commitHashDecoder body of
      Right hash ->
        return $ Right hash

      Left _ ->
        return $ Left (Exit.PublishMissingTag vsn)


toTagUrl :: Pkg.Name -> V.Version -> String
toTagUrl pkg vsn =
  "https://api.github.com/repos/" ++ Pkg.toUrl pkg ++ "/git/refs/tags/" ++ V.toChars vsn


commitHashDecoder :: D.Decoder e String
commitHashDecoder =
  Utf8.toChars <$>
    D.field "object" (D.field "sha" D.string)



-- VERIFY NO LOCAL CHANGES SINCE TAG


verifyNoChanges :: String -> V.Version -> Task.Task Exit.Publish ()
verifyNoChanges commitHash vsn =
  reportChangesPhase $
  do  maybeGit <- Dir.findExecutable "git"
      case maybeGit of
        Nothing ->
          return $ Left Exit.PublishNoGit

        Just git ->
          do  -- https://stackoverflow.com/questions/3878624/how-do-i-programmatically-determine-if-there-are-uncommited-changes
              exitCode <- Process.rawSystem git [ "diff-index", "--quiet", commitHash, "--" ]
              case exitCode of
                Exit.ExitSuccess   -> return $ Right ()
                Exit.ExitFailure _ -> return $ Left (Exit.PublishLocalChanges vsn)



-- VERIFY THAT ZIP BUILDS / COMPUTE HASH


verifyZip :: Http.Manager -> Pkg.Name -> V.Version -> Task.Task Exit.Publish Http.Sha
verifyZip manager pkg vsn =
  withTempDir $ \prepublishDir ->
    do  (sha, archive) <-
          reportDownloadPhase $
            Http.getArchive manager (toZipUrl pkg vsn)
              Exit.PublishCannotGetZip
              Exit.PublishCannotDecodeZip
              (return . Right)

        Task.io $ File.writePackage prepublishDir archive

        reportBuildPhase $
          Dir.withCurrentDirectory prepublishDir $
            do  error "TODO Build.generateDocs"
                return ()

        return sha


withTempDir :: FilePath -> (FilePath -> IO a) -> IO a
withTempDir root callback =
  let
    dir = Stuff.prepublishDir root
  in
  bracket_
    (Dir.createDirectoryIfMissing True dir)
    (Dir.removeDirectoryRecursive dir)
    (callback dir)


toZipUrl :: Pkg.Name -> V.Version -> String
toZipUrl pkg vsn =
  "https://github.com/" ++ Pkg.toUrl pkg ++ "/zipball/" ++ V.toChars vsn ++ "/"



-- VERIFY VERSION


data GoodVersion
  = GoodStart
  | GoodBump V.Version M.Magnitude


verifyVersion :: Env -> Pkg.Name -> V.Version -> Docs.Documentation -> Maybe Registry.KnownVersions -> Task.Task Exit.Publish ()
verifyVersion env pkg vsn newDocs publishedVersions =
  reportVersionPhase vsn $
    case publishedVersions of
      Nothing ->
        if vsn == V.one
        then return $ Right GoodStart
        else return $ Left $ Exit.PublishNotInitialVersion vsn

      Just knownVersions@(Registry.KnownVersions latest previous) ->
        if vsn == latest || elem vsn previous
        then return $ Left $ Exit.PublishAlreadyPublished vsn
        else verifyBump env pkg vsn newDocs knownVersions


verifyBump :: Env -> Pkg.Name -> V.Version -> Docs.Documentation -> Registry.KnownVersions -> IO (Either Exit.Publish GoodVersion)
verifyBump (Env _ cache manager _ _) pkg vsn newDocs knownVersions@(Registry.KnownVersions latest _) =
  case List.find (\(_ ,new, _) -> vsn == new) (Bump.getPossibilities knownVersions) of
    Nothing ->
      return $ Left $
        Exit.PublishInvalidBump vsn latest

    Just (old, new, magnitude) ->
      do  result <- Diff.getDocs cache manager pkg old
          case result of
            Left dp ->
              return $ Left $ Exit.PublishCannotGetDocs dp

            Right oldDocs ->
              let
                changes = Diff.diff oldDocs newDocs
                realNew = Diff.bump changes old
              in
              if new == realNew
              then return $ Right $ GoodBump old magnitude
              else
                return $ Left $
                  Exit.PublishBadBump old new magnitude realNew (Diff.toMagnitude changes)



-- REGISTER PACKAGES


register :: Http.Manager -> Pkg.Name -> V.Version -> String -> Http.Sha -> Task.Task Exit.Publish ()
register manager pkg vsn commitHash sha =
  let
    url =
      Website.route "/register"
        [ ("name", Pkg.toChars pkg)
        , ("version", V.toChars vsn)
        , ("commit-hash", commitHash)
        ]
  in
  Task.eio Exit.PublishCannotRegister $
    Http.upload manager url
      [ Http.filePart "elm.json" "elm.json"
      , Http.filePart "docs.json" (error "TODO generate docs, maybe in memory only?")
      , Http.filePart "README.md" "README.md"
      , Http.stringPart "github-hash" (Http.shaToChars sha)
      ]



-- REPORTING


reportPublishStart :: Pkg.Name -> V.Version -> Maybe Registry.KnownVersions -> IO ()
reportPublishStart pkg vsn maybeKnownVersions =
  case maybeKnownVersions of
    Nothing ->
      putStrLn $ Exit.newPackageOverview ++ "\nI will now verify that everything is in order...\n"

    Just _ ->
      putStrLn $ "Verifying" ++ Pkg.toChars pkg ++ V.toChars vsn ++ "...\n"



-- REPORTING PHASES


reportReadmePhase :: IO (Either x a) -> Task.Task x a
reportReadmePhase =
  reportPhase
    "Looking for README.md"
    "Found README.md"
    "Problem with your README.md"


reportLicensePhase :: IO (Either x a) -> Task.Task x a
reportLicensePhase =
  reportPhase
    "Looking for LICENSE"
    "Found LICENSE"
    "Problem with your LICENSE"


reportVersionPhase :: V.Version -> IO (Either x GoodVersion) -> Task.Task x ()
reportVersionPhase version work =
  let
    vsn = V.toChars version

    waiting = "Checking semantic versioning rules. Is " ++ vsn ++ " correct?"
    failure = "Version " ++ vsn ++ " is not correct!"
    success result =
      case result of
        GoodStart ->
          "All packages start at version " ++ V.toChars V.one

        GoodBump oldVersion magnitude ->
          "Version number " ++ vsn ++ " verified ("
          ++ M.toChars magnitude ++ " change, "
          ++ V.toChars oldVersion ++ " => " ++ vsn ++ ")"
  in
  void $ reportCustomPhase waiting success failure work


reportTagPhase :: V.Version -> IO (Either x a) -> Task.Task x a
reportTagPhase vsn =
  reportPhase
    ("Is version " ++ V.toChars vsn ++ " tagged on GitHub?")
    ("Version " ++ V.toChars vsn ++ " is tagged on GitHub")
    ("Version " ++ V.toChars vsn ++ " is not tagged on GitHub!")


reportDownloadPhase :: IO (Either x a) -> Task.Task x a
reportDownloadPhase =
  reportPhase
    "Downloading code from GitHub..."
    "Code downloaded successfully from GitHub"
    "Could not download code from GitHub!"


reportBuildPhase :: IO (Either x a) -> Task.Task x a
reportBuildPhase =
  reportPhase
    "Building downloaded code and generated docs..."
    "Downloaded code compiles successfully / docs generated\n"
    "Cannot compile downloaded code!"


reportChangesPhase :: IO (Either x a) -> Task.Task x a
reportChangesPhase =
  reportPhase
    "Checking for uncommitted changes..."
    "No uncommitted changes in local code"
    "Your local code is different than the code tagged on GitHub"


reportPhase :: String -> String -> String -> IO (Either x a) -> Task.Task x a
reportPhase waiting success failure work =
  reportCustomPhase waiting (\_ -> success) failure work


reportCustomPhase :: String -> (a -> String) -> String -> IO (Either x a) -> Task.Task x a
reportCustomPhase waiting success failure work =
  let
    putFlush doc =
      Help.toStdout doc >> IO.hFlush IO.stdout

    padded message =
      message ++ replicate (length waiting - length message) ' '
  in
  Task.eio id $
  do  putFlush $ "  " <> waitingMark <+> D.fromChars waiting
      result <- work
      putFlush $
        case result of
          Right a -> "\r  " <> goodMark <+> D.fromChars (padded (success a) ++ "\n")
          Left _  -> "\r  " <> badMark  <+> D.fromChars (padded failure ++ "\n\n")

      return result


-- MARKS


goodMark :: D.Doc
goodMark =
  D.green $ if isWindows then "+" else "●"


badMark :: D.Doc
badMark =
  D.red $ if isWindows then "X" else "✗"


waitingMark :: D.Doc
waitingMark =
  D.dullyellow $ if isWindows then "-" else "→"


isWindows :: Bool
isWindows =
  Info.os == "mingw32"
