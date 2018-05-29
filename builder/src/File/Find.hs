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
import qualified System.FilePath as FP
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))

import qualified Elm.Compiler.Module as Module
import qualified Elm.Name as N
import qualified Elm.Package as Pkg

import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified Reporting.Exit.Crawl as E
import qualified Reporting.Task as Task



-- ASSET


data Asset
  = Local FilePath
  | Kernel FilePath (Maybe FilePath)
  | Foreign Pkg.Package
  | ForeignKernel



-- FIND


find :: Summary.Summary -> E.Origin -> Module.Raw -> Task.Task_ E.Problem Asset
find (Summary.Summary root project exposed _ _) origin name =
  do  here <- liftIO Dir.getCurrentDirectory
      let toRoot dir = FP.makeRelative here (root </> dir)
      case project of
        Project.App info ->
          do  let srcDirs = map toRoot (Project._app_source_dirs info)
              findElm project srcDirs exposed origin name

        Project.Pkg _ ->
          if N.startsWith "Elm.Kernel." name then
            if Project.isPlatformPackage project then
              findKernel (toRoot "src") exposed origin name
            else
              Task.throw $ E.ModuleNameReservedForKernel origin name

          else
            findElm project [ toRoot "src" ] exposed origin name



-- FIND ELM


findElm :: Project.Project -> [FilePath] -> Summary.ExposedModules -> E.Origin -> Module.Raw -> Task.Task_ E.Problem Asset
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


elmExists :: Module.Raw -> FilePath -> IO (Maybe FilePath)
elmExists name srcDir =
  do  let path = srcDir </> Module.nameToSlashPath name <.> "elm"
      exists <- Dir.doesFileExist path
      return $ if exists then Just path else Nothing



-- FIND KERNEL


findKernel :: FilePath -> Summary.ExposedModules -> E.Origin -> Module.Raw -> Task.Task_ E.Problem Asset
findKernel srcDir exposed origin name =
  do  let clientPath = srcDir </> Module.nameToSlashPath name <.> "js"
      let serverPath = srcDir </> Module.nameToSlashPath name <.> "server.js"
      client <- liftIO $ Dir.doesFileExist clientPath
      server <- liftIO $ Dir.doesFileExist serverPath
      if client
        then return $ Kernel clientPath (if server then Just serverPath else Nothing)
        else
          case Map.lookup (N.drop 11 name) exposed of
            Just [Pkg.Package pkg _vsn] | pkg == Pkg.core || pkg == Pkg.virtualDom ->
              return ForeignKernel

            _ ->
              Task.throw $ E.ModuleNotFound origin name E.Pkg
