{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module File.Find
  ( Asset(..)
  , find
  )
  where

import Control.Monad.Except (liftIO)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import qualified System.FilePath as FP
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg

import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified Reporting.Exit.Crawl as E
import qualified Reporting.Task as Task



-- ASSET


data Asset
  = Local FilePath
  | Kernel FilePath (Maybe FilePath)
  | Foreign Pkg.Canonical
  | ForeignKernel



-- FIND


find :: Summary.Summary -> E.Origin -> ModuleName.Raw -> Task.Task_ E.Problem Asset
find (Summary.Summary root project exposed _ _) origin name =
  do  here <- liftIO Dir.getCurrentDirectory
      let toRoot dir = FP.makeRelative here (root </> dir)
      case project of
        Project.App info ->
          do  let srcDirs = map toRoot (Project._app_source_dirs info)
              findElm project srcDirs exposed origin name

        Project.Pkg _ ->
          if Name.isKernel name then
            if Project.isPlatformPackage project then
              findKernel (toRoot "src") exposed origin name
            else
              Task.throw $ E.ModuleNameReservedForKernel origin name

          else
            findElm project [ toRoot "src" ] exposed origin name



-- FIND ELM


findElm :: Project.Project -> [FilePath] -> Summary.ExposedModules -> E.Origin -> ModuleName.Raw -> Task.Task_ E.Problem Asset
findElm project srcDirs exposed origin name =
  do
      paths <- liftIO $ Maybe.catMaybes <$> mapM (elmExists name) srcDirs

      case (paths, Map.lookup name exposed) of
        ([path], Nothing) ->
            return (Local path)

        ([], Just [pkg]) ->
            return (Foreign pkg)

        ([], Nothing) ->
            case project of
              Project.App _ ->
                Task.throw $ E.ModuleNotFound origin name (E.App srcDirs)

              Project.Pkg _ ->
                Task.throw $ E.ModuleNotFound origin name E.Pkg

        (_, maybePkgs) ->
            Task.throw $ E.ModuleAmbiguous origin name paths (maybe [] id maybePkgs)


elmExists :: ModuleName.Raw -> FilePath -> IO (Maybe FilePath)
elmExists name srcDir =
  do  let path = srcDir </> ModuleName.nameToSlashPath name <.> "elm"
      exists <- Dir.doesFileExist path
      return $ if exists then Just path else Nothing



-- FIND KERNEL


findKernel :: FilePath -> Summary.ExposedModules -> E.Origin -> ModuleName.Raw -> Task.Task_ E.Problem Asset
findKernel srcDir exposed origin name =
  do  let clientPath = srcDir </> ModuleName.nameToSlashPath name <.> "js"
      let serverPath = srcDir </> ModuleName.nameToSlashPath name <.> "server.js"
      client <- liftIO $ Dir.doesFileExist clientPath
      server <- liftIO $ Dir.doesFileExist serverPath
      if client
        then return $ Kernel clientPath (if server then Just serverPath else Nothing)
        else
          case Map.lookup (Name.getKernel name) exposed of
            Just [Pkg.Canonical pkg _vsn] | pkg == Pkg.core || pkg == Pkg.virtualDom ->
              return ForeignKernel

            _ ->
              Task.throw $ E.ModuleNotFound origin name E.Pkg
