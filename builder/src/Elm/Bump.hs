{-# LANGUAGE OverloadedStrings #-}
module Elm.Bump
  ( bump
  , toPossibleBumps
  )
  where


import Control.Monad.Trans (liftIO)
import qualified Data.List as List

import qualified Deps.Cache as Cache
import qualified Deps.Diff as Diff
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified Elm.Version as V
import Reporting.Doc ((<>), (<+>))
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Bump as E
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Progress.Terminal as Terminal
import qualified Reporting.Task as Task



-- BUMP


bump :: Summary.Summary -> Task.Task ()
bump summary@(Summary.Summary root project _ _ _) =
  case project of
    Project.App _ ->
      Task.throw (Exit.Bump E.Application)

    Project.Pkg info@(Project.PkgInfo name _ _ version _ _ _ _) ->
      do  registry <- Cache.mandatoryUpdate
          case Cache.getVersions name registry of
            Left _suggestions ->
              checkNewPackage root info

            Right publishedVersions ->
              let
                bumpableVersions =
                  map (\(old, _, _) -> old) (toPossibleBumps publishedVersions)
              in
                if elem version bumpableVersions then
                  suggestVersion summary info
                else
                  Task.throw $ Exit.Bump $ E.Unbumpable version $
                    map head (List.group (List.sort bumpableVersions))



-- VALID BUMPS


toPossibleBumps :: [V.Version] -> [(V.Version, V.Version, Diff.Magnitude)]
toPossibleBumps publishedVersions =
  case publishedVersions of
    [] ->
      []

    _ ->
      let
        patchPoints =
          V.filterLatest V.majorAndMinor publishedVersions

        minorPoints =
          V.filterLatest V._major publishedVersions

        majorPoint =
          maximum publishedVersions
      in
        (majorPoint, V.bumpMajor majorPoint, Diff.MAJOR)
        :  map (\v -> (v, V.bumpMinor v, Diff.MINOR)) minorPoints
        ++ map (\v -> (v, V.bumpPatch v, Diff.PATCH)) patchPoints



-- CHECK NEW PACKAGE


checkNewPackage :: FilePath -> Project.PkgInfo -> Task.Task ()
checkNewPackage root info@(Project.PkgInfo _ _ _ version _ _ _ _) =
  do  liftIO $ putStrLn Terminal.newPackageOverview
      if version == V.one
        then
          liftIO $ putStrLn "The version number in elm.json is correct so you are all set!"
        else
          changeVersion root info V.one $
            "It looks like the version in elm.json has been changed though!\n\
            \Would you like me to change it back to "
            <> D.fromUtf8 (V.toString V.one) <> "? [Y/n] "



-- SUGGEST VERSION


suggestVersion :: Summary.Summary -> Project.PkgInfo -> Task.Task ()
suggestVersion (Summary.Summary root _ _ _ _) info@(Project.PkgInfo name _ _ version _ _ _ _) =
  do  oldDocs <- Cache.getDocs name version
      newDocs <- Task.silently (error "TODO Project.generateDocs summary")
      let changes = Diff.diff oldDocs newDocs
      let newVersion = Diff.bump changes version
      changeVersion root info newVersion $
        let
          old = D.fromUtf8 $ V.toString version
          new = D.fromUtf8 $ V.toString newVersion
          mag = D.fromChars $ Diff.magnitudeToString (Diff.toMagnitude changes)
        in
          "Based on your new API, this should be a" <+> D.green mag <+> "change (" <> old <> " => " <> new <> ")\n"
          <> "Bail out of this command and run 'elm diff' for a full explanation.\n"
          <> "\n"
          <> "Should I perform the update (" <> old <> " => " <> new <> ") in elm.json? [Y/n] "



-- CHANGE VERSION


changeVersion :: FilePath -> Project.PkgInfo -> V.Version -> D.Doc -> Task.Task ()
changeVersion root info targetVersion explanation =
  do  approved <- Task.getApproval explanation
      if not approved
        then
          liftIO $ putStrLn "Okay, I did not change anything!"

        else
          do  liftIO $ Project.write root $ Project.Pkg $
                info { Project._pkg_version = targetVersion }

              liftIO $ Help.toStdout $
                "Version changed to "
                <> D.green (D.fromUtf8 (V.toString targetVersion))
                <> "!\n"
