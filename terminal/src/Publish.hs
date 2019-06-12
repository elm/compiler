{-# LANGUAGE OverloadedStrings #-}
module Publish
  ( run
  )
  where


import Control.Exception (bracket_)
import Control.Monad (void)
import qualified Data.List as List
import qualified Data.NonEmptyList as NE
import qualified Data.Utf8 as Utf8
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.Info as Info
import qualified System.IO as IO
import qualified System.Process as Process

import qualified Build
import qualified Deps.Bump as Bump
import qualified Deps.Diff as Diff
import qualified Deps.Registry as Registry
import qualified Deps.Website as Website
import qualified Elm.Details as Details
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
publish env@(Env root _ manager registry outline) =
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
          docs <- verifyBuild root
          verifyVersion env pkg vsn docs maybeKnownVersions
          commitHash <- verifyTag manager pkg vsn
          verifyNoChanges commitHash vsn
          zipHash <- verifyZip env pkg vsn

          Task.io $ putStrLn ""
          register manager pkg vsn docs commitHash zipHash
          Task.io $ putStrLn "Success!"



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
  reportReadmeCheck $
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
  reportLicenseCheck $
  do  let licensePath = root </> "LICENSE"
      exists <- File.exists licensePath
      if exists
        then return (Right ())
        else return (Left Exit.PublishNoLicense)



-- VERIFY BUILD


verifyBuild :: FilePath -> Task.Task Exit.Publish Docs.Documentation
verifyBuild root =
  reportBuildCheck $ Task.run $
    do  details@(Details.Details _ outline _ _ _) <-
          Task.eio Exit.PublishBadDetails $
            Details.load Reporting.silent root

        exposed <-
          case outline of
            Details.ValidApp _        -> Task.throw Exit.PublishApplication
            Details.ValidPkg _ []     -> Task.throw Exit.PublishNoExposed
            Details.ValidPkg _ (e:es) -> return (NE.List e es)

        Task.eio Exit.PublishBuildProblem $
          Build.fromExposed Reporting.silent root details Build.KeepDocs exposed



-- VERIFY GITHUB TAG


verifyTag :: Http.Manager -> Pkg.Name -> V.Version -> Task.Task Exit.Publish String
verifyTag manager pkg vsn =
  reportTagCheck vsn $
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
  reportLocalChangesCheck $
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


verifyZip :: Env -> Pkg.Name -> V.Version -> Task.Task Exit.Publish Http.Sha
verifyZip (Env root _ manager _ _) pkg vsn =
  withTempDir root $ \prepublishDir ->
    do  (sha, archive) <-
          reportDownloadCheck $
            Http.getArchive manager (toZipUrl pkg vsn)
              Exit.PublishCannotGetZip
              Exit.PublishCannotDecodeZip
              (return . Right)

        Task.io $ File.writePackage prepublishDir archive

        reportZipBuildCheck $
          Dir.withCurrentDirectory prepublishDir $
            verifyZipBuild prepublishDir

        return sha


toZipUrl :: Pkg.Name -> V.Version -> String
toZipUrl pkg vsn =
  "https://github.com/" ++ Pkg.toUrl pkg ++ "/zipball/" ++ V.toChars vsn ++ "/"


withTempDir :: FilePath -> (FilePath -> Task.Task x a) -> Task.Task x a
withTempDir root callback =
  let
    dir = Stuff.prepublishDir root
  in
  Task.eio id $
    bracket_
      (Dir.createDirectoryIfMissing True dir)
      (Dir.removeDirectoryRecursive dir)
      (Task.run (callback dir))


verifyZipBuild :: FilePath -> IO (Either Exit.Publish ())
verifyZipBuild root =
  Task.run $
  do  details@(Details.Details _ outline _ _ _) <-
        Task.eio Exit.PublishZipBadDetails $
          Details.load Reporting.silent root

      exposed <-
        case outline of
          Details.ValidApp _        -> Task.throw Exit.PublishZipApplication
          Details.ValidPkg _ []     -> Task.throw Exit.PublishZipNoExposed
          Details.ValidPkg _ (e:es) -> return (NE.List e es)

      _ <- Task.eio Exit.PublishZipBuildProblem $
        Build.fromExposed Reporting.silent root details Build.KeepDocs exposed

      return ()



-- VERIFY VERSION


data GoodVersion
  = GoodStart
  | GoodBump V.Version M.Magnitude


verifyVersion :: Env -> Pkg.Name -> V.Version -> Docs.Documentation -> Maybe Registry.KnownVersions -> Task.Task Exit.Publish ()
verifyVersion env pkg vsn newDocs publishedVersions =
  reportSemverCheck vsn $
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


register :: Http.Manager -> Pkg.Name -> V.Version -> Docs.Documentation -> String -> Http.Sha -> Task.Task Exit.Publish ()
register manager pkg vsn docs commitHash sha =
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
      , Http.jsonPart "docs.json" "docs.json" (Docs.encode docs)
      , Http.filePart "README.md" "README.md"
      , Http.stringPart "github-hash" (Http.shaToChars sha)
      ]



-- REPORTING


reportPublishStart :: Pkg.Name -> V.Version -> Maybe Registry.KnownVersions -> Task.Task x ()
reportPublishStart pkg vsn maybeKnownVersions =
  Task.io $
  case maybeKnownVersions of
    Nothing ->
      putStrLn $ Exit.newPackageOverview ++ "\nI will now verify that everything is in order...\n"

    Just _ ->
      putStrLn $ "Verifying" ++ Pkg.toChars pkg ++ V.toChars vsn ++ "...\n"



-- REPORTING PHASES


reportReadmeCheck :: IO (Either x a) -> Task.Task x a
reportReadmeCheck =
  reportCheck
    "Looking for README.md"
    "Found README.md"
    "Problem with your README.md"


reportLicenseCheck :: IO (Either x a) -> Task.Task x a
reportLicenseCheck =
  reportCheck
    "Looking for LICENSE"
    "Found LICENSE"
    "Problem with your LICENSE"


reportBuildCheck :: IO (Either x a) -> Task.Task x a
reportBuildCheck =
  reportCheck
    "Verifying package documentation..."
    "Verified package documentation"
    "Problem with package documentation"


reportSemverCheck :: V.Version -> IO (Either x GoodVersion) -> Task.Task x ()
reportSemverCheck version work =
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
  void $ reportCustomCheck waiting success failure work


reportTagCheck :: V.Version -> IO (Either x a) -> Task.Task x a
reportTagCheck vsn =
  reportCheck
    ("Is version " ++ V.toChars vsn ++ " tagged on GitHub?")
    ("Version " ++ V.toChars vsn ++ " is tagged on GitHub")
    ("Version " ++ V.toChars vsn ++ " is not tagged on GitHub!")


reportDownloadCheck :: IO (Either x a) -> Task.Task x a
reportDownloadCheck =
  reportCheck
    "Downloading code from GitHub..."
    "Code downloaded successfully from GitHub"
    "Could not download code from GitHub!"


reportLocalChangesCheck :: IO (Either x a) -> Task.Task x a
reportLocalChangesCheck =
  reportCheck
    "Checking for uncommitted changes..."
    "No uncommitted changes in local code"
    "Your local code is different than the code tagged on GitHub"


reportZipBuildCheck :: IO (Either x a) -> Task.Task x a
reportZipBuildCheck =
  reportCheck
    "Verifying downloaded code..."
    "Downloaded code compiles successfully"
    "Cannot compile downloaded code!"


reportCheck :: String -> String -> String -> IO (Either x a) -> Task.Task x a
reportCheck waiting success failure work =
  reportCustomCheck waiting (\_ -> success) failure work


reportCustomCheck :: String -> (a -> String) -> String -> IO (Either x a) -> Task.Task x a
reportCustomCheck waiting success failure work =
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
