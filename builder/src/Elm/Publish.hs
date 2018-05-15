{-# OPTIONS_GHC -Wall #-}
module Elm.Publish (publish) where


import Control.Monad (when)
import Control.Monad.Except (catchError, runExceptT)
import Control.Monad.Trans (liftIO, lift)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Process as Process

import qualified Deps.Cache as Cache
import qualified Deps.Diff as Diff
import qualified Deps.Website as Website
import qualified Elm.Bump as Bump
import qualified Elm.Docs as Docs
import qualified Elm.Package as Pkg
import qualified Elm.Project as Project
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Publish as E
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task
import qualified Stuff.Paths as Path



-- PUBLISH


publish :: Summary.Summary -> Task.Task ()
publish summary@(Summary.Summary root project _ _ _) =
  case project of
    Project.App _ ->
      throw E.Application

    Project.Pkg (Project.PkgInfo name smry _ version exposed _ _ _) ->
      do
          registry <- Cache.mandatoryUpdate
          let maybePublishedVersions = either (const Nothing) Just (Cache.getVersions name registry)

          Task.report (Progress.PublishStart name version maybePublishedVersions)

          when (noExposed exposed) $ throw E.NoExposed
          when (badSummary smry)   $ throw E.NoSummary

          verifyReadme root
          verifyLicense root
          docs <- Task.silently (Project.generateDocs summary)
          verifyVersion name version docs maybePublishedVersions
          commitHash <- verifyTag name version
          verifyNoChanges commitHash version
          zipHash <- verifyZip name version

          Website.register name version commitHash zipHash

          Task.report Progress.PublishEnd


throw :: E.Exit -> Task.Task a
throw exit =
  Task.throw (Exit.Publish exit)



-- VERIFY SUMMARY


badSummary :: Text.Text -> Bool
badSummary summary =
  Text.null summary || Project.defaultSummary == summary


noExposed :: Project.Exposed -> Bool
noExposed exposed =
  case exposed of
    Project.ExposedList modules ->
      null modules

    Project.ExposedDict chunks ->
      all (null . snd) chunks



-- VERIFY README


verifyReadme :: FilePath -> Task.Task ()
verifyReadme root =
  phase Progress.CheckReadme $
    do  let readmePath = root </> "README.md"
        exists <- liftIO $ Dir.doesFileExist readmePath
        case exists of
          False ->
            throw E.NoReadme

          True ->
            do  size <- liftIO $ IO.withFile readmePath IO.ReadMode IO.hFileSize
                if size < 300
                  then throw E.ShortReadme
                  else return ()



-- VERIFY LICENSE


verifyLicense :: FilePath -> Task.Task ()
verifyLicense root =
  phase Progress.CheckLicense $
    do  let licensePath = root </> "LICENSE"
        exists <- liftIO $ Dir.doesFileExist licensePath
        if exists
          then return ()
          else throw E.NoLicense



-- VERIFY GITHUB TAG


verifyTag :: Pkg.Name -> Pkg.Version -> Task.Task String
verifyTag name version =
  phase (Progress.CheckTag version) $
    Website.githubCommit name version `catchError` \_ ->
      throw (E.MissingTag version)



-- VERIFY NO LOCAL CHANGES SINCE TAG


verifyNoChanges :: String -> Pkg.Version -> Task.Task ()
verifyNoChanges commitHash version =
  phase Progress.CheckChanges $
    do  maybeGit <- liftIO $ Dir.findExecutable "git"
        case maybeGit of
          Nothing ->
            throw E.NoGit

          Just git ->
            do  -- https://stackoverflow.com/questions/3878624/how-do-i-programmatically-determine-if-there-are-uncommited-changes
                exitCode <- liftIO $
                  Process.rawSystem git [ "diff-index", "--quiet", commitHash, "--" ]

                case exitCode of
                  Exit.ExitSuccess ->
                    return ()

                  Exit.ExitFailure _ ->
                    throw (E.LocalChanges version)



-- VERIFY THAT ZIP BUILDS / COMPUTE HASH


verifyZip :: Pkg.Name -> Pkg.Version -> Task.Task Website.Sha
verifyZip name version =
  withTempDir $ \dir ->
    do  hash <- phase Progress.CheckDownload $
          Website.githubDownload name version dir

        phase Progress.CheckBuild $
          do  runner <- Task.getSilentRunner
              result <- liftIO $ Dir.withCurrentDirectory dir $ runner $
                Task.silently $ Project.generateDocs =<< Project.getRoot
              either Task.throw (\_ -> return ()) result

        return hash


withTempDir :: (FilePath -> Task.Task a) -> Task.Task a
withTempDir callback =
  do  liftIO $ Dir.createDirectoryIfMissing True Path.prepublishDir
      result <- lift $ runExceptT $ callback Path.prepublishDir
      liftIO $ Dir.removeDirectoryRecursive Path.prepublishDir
      either Task.throw return result



-- PHASE REPORTING


phase :: Progress.PublishPhase -> Task.Task a -> Task.Task a
phase publishPhase task =
  do  Task.report $ Progress.PublishProgress publishPhase Nothing

      result <- task `catchError` \exit ->
        do  Task.report $ Progress.PublishProgress publishPhase (Just Progress.Bad)
            Task.throw exit

      Task.report $ Progress.PublishProgress publishPhase (Just Progress.Good)

      return result



-- VERIFY VERSION


verifyVersion :: Pkg.Name -> Pkg.Version -> Docs.Documentation -> Maybe [Pkg.Version] -> Task.Task ()
verifyVersion name version docs maybePublishedVersions =
  let
    reportBumpPhase bumpPhase =
      Task.report $ Progress.PublishCheckBump version bumpPhase
  in
  do  reportBumpPhase Progress.StatedVersion
      case maybePublishedVersions of
        Nothing ->
          if version == Pkg.initialVersion then
            reportBumpPhase Progress.GoodStart
          else
            throw (E.NotInitialVersion version)

        Just publishedVersions ->
          if elem version publishedVersions then
            throw $ E.AlreadyPublished version
          else
            do  (old, magnitude) <- verifyBump name version docs publishedVersions
                reportBumpPhase (Progress.GoodBump old magnitude)

  `catchError` \exit ->
    do  reportBumpPhase Progress.BadBump
        Task.throw exit


verifyBump :: Pkg.Name -> Pkg.Version -> Docs.Documentation -> [Pkg.Version] -> Task.Task (Pkg.Version, Diff.Magnitude)
verifyBump name statedVersion newDocs publishedVersions =
  let
    possibleBumps =
      Bump.toPossibleBumps publishedVersions

    isTheBump (_ ,new, _) =
      statedVersion == new
  in
  case List.find isTheBump possibleBumps of
    Nothing ->
      throw $ E.InvalidBump statedVersion (last publishedVersions)

    Just (old, new, magnitude) ->
      do  oldDocs <- Cache.getDocs name old
          let changes = Diff.diff oldDocs newDocs
          let realNew = Diff.bump changes old
          if new == realNew
            then
              return (old, magnitude)
            else
              throw $ E.BadBump old new magnitude realNew $
                Diff.toMagnitude changes
