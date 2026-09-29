{-# OPTIONS_GHC -fno-warn-x-partial #-}
{-# LANGUAGE OverloadedStrings #-}
module Bump
  ( run
  )
  where


import qualified Data.List as List
import qualified Data.NonEmptyList as NE

import qualified File

import qualified Build
import qualified Deps.Bump as Bump
import qualified Deps.Diff as Diff
import qualified Deps.Registry as Registry
import qualified Elm.Details as Details
import qualified Elm.Docs as Docs
import qualified Elm.Magnitude as M
import qualified Elm.Outline as Outline
import qualified Elm.Version as V
import qualified Http
import Reporting.Doc ((<+>))
import qualified Reporting
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Task as Task
import qualified Root as R



-- RUN


run :: () -> () -> IO ()
run () () =
  Reporting.attempt Exit.bumpToReport $
    do  maybeRoot <- R.findRoot
        case maybeRoot of
          Nothing   -> return $ Left Exit.BumpNoOutline
          Just root ->
            R.withRootLock root $ \writer stuff ->
              Task.run (bump writer =<< getEnv root stuff)



-- ENV


data Env =
  Env
    { _root :: R.Root
    , _stuff :: R.Stuff
    , _cache :: R.PackageCache
    , _manager :: Http.Manager
    , _registry :: Registry.Registry
    , _outline :: Outline.PkgOutline
    }


getEnv :: R.Root -> R.Stuff -> Task.Task Exit.Bump Env
getEnv root stuff =
  do  cache <- Task.io $ R.getPackageCache
      manager <- Task.io $ Http.getManager
      registry <- Task.eio Exit.BumpMustHaveLatestRegistry $ R.withRegistryLock cache $ \writer -> Registry.latest writer manager cache
      outline <- Task.eio Exit.BumpBadOutline $ Outline.read root
      case outline of
        Outline.App _ ->
          Task.throw Exit.BumpApplication

        Outline.Pkg pkgOutline ->
          return $ Env root stuff cache manager registry pkgOutline



-- BUMP


bump :: File.Writer R.PROJECT -> Env -> Task.Task Exit.Bump ()
bump writer env@(Env root _ _ _ registry outline@(Outline.PkgOutline pkg _ _ vsn _ _ _ _)) =
  case Registry.getVersions pkg registry of
    Just knownVersions ->
      let
        bumpableVersions =
          map (\(old, _, _) -> old) (Bump.getPossibilities knownVersions)
      in
      if elem vsn bumpableVersions
      then suggestVersion writer env
      else
        Task.throw $ Exit.BumpUnexpectedVersion vsn $
          map head (List.group (List.sort bumpableVersions))

    Nothing ->
      Task.io $ checkNewPackage writer root outline



-- CHECK NEW PACKAGE


checkNewPackage :: File.Writer R.PROJECT -> R.Root -> Outline.PkgOutline -> IO ()
checkNewPackage writer root outline@(Outline.PkgOutline _ _ _ version _ _ _ _) =
  do  putStrLn Exit.newPackageOverview
      if version == V.one
        then
          putStrLn "The version number in elm.json is correct so you are all set!"
        else
          changeVersion writer root outline V.one $
            "It looks like the version in elm.json has been changed though!\n\
            \Would you like me to change it back to "
            <> D.fromVersion V.one <> "? [Y/n] "



-- SUGGEST VERSION


suggestVersion :: File.Writer R.PROJECT -> Env -> Task.Task Exit.Bump ()
suggestVersion writer (Env root stuff cache manager _ outline@(Outline.PkgOutline pkg _ _ vsn _ _ _ _)) =
  do  oldDocs <- Task.eio (Exit.BumpCannotFindDocs pkg vsn) $ R.withRegistryLock cache $ \w -> Diff.getDocs w cache manager pkg vsn
      newDocs <- generateDocs writer root stuff outline
      let changes = Diff.diff oldDocs newDocs
      let newVersion = Diff.bump changes vsn
      Task.io $ changeVersion writer root outline newVersion $
        let
          old = D.fromVersion vsn
          new = D.fromVersion newVersion
          mag = D.fromChars $ M.toChars (Diff.toMagnitude changes)
        in
        "Based on your new API, this should be a" <+> D.green mag <+> "change (" <> old <> " => " <> new <> ")\n"
        <> "Bail out of this command and run 'elm diff' for a full explanation.\n"
        <> "\n"
        <> "Should I perform the update (" <> old <> " => " <> new <> ") in elm.json? [Y/n] "


generateDocs :: File.Writer R.PROJECT -> R.Root -> R.Stuff -> Outline.PkgOutline -> Task.Task Exit.Bump Docs.Documentation
generateDocs writer root stuff (Outline.PkgOutline _ _ _ _ exposed _ _ _) =
  do  details <-
        Task.eio Exit.BumpBadDetails $
          Details.load writer Reporting.silent root stuff

      case Outline.flattenExposed exposed of
        [] ->
          Task.throw $ Exit.BumpNoExposed

        e:es ->
          Task.eio Exit.BumpBadBuild $
            Build.fromExposed writer Reporting.silent root stuff details Build.KeepDocs (NE.List e es)



-- CHANGE VERSION


changeVersion :: File.Writer R.PROJECT -> R.Root -> Outline.PkgOutline -> V.Version -> D.Doc -> IO ()
changeVersion writer root outline targetVersion question =
  do  approved <- Reporting.ask question
      if not approved
        then
          putStrLn "Okay, I did not change anything!"

        else
          do  Outline.write writer root $ Outline.Pkg $
                outline { Outline._pkg_version = targetVersion }

              Help.toStdout $
                "Version changed to "
                <> D.green (D.fromVersion targetVersion)
                <> "!\n"
