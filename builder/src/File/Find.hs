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
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))

import qualified Elm.Compiler.Module as Module
import qualified Elm.Name as N
import qualified Elm.Package as Pkg

import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified Reporting.Error.Crawl as E
import qualified Reporting.Task as Task



-- ASSET


data Asset
  = Local FilePath
  | Kernel FilePath (Maybe FilePath)
  | Foreign Pkg.Package



-- FIND


find :: Summary.Summary -> E.Origin -> Module.Raw -> Task.Task_ E.Problem Asset
find (Summary.Summary root project exposed _ _) origin name =
  case project of
    Project.App info ->
      do  let srcDirs = map (root </>) (Project._app_source_dirs info)
          findElm srcDirs exposed origin name

    Project.Pkg _ ->
      if N.startsWith "Elm.Kernel." name then
        if Project.isPlatformPackage project then
          findKernel (root </> "src") origin name
        else
          Task.throw $ E.ModuleNameReservedForKernel origin name

      else
        findElm [ root </> "src" ] exposed origin name



-- FIND ELM


findElm :: [FilePath] -> Summary.ExposedModules -> E.Origin -> Module.Raw -> Task.Task_ E.Problem Asset
findElm srcDirs exposed origin name =
  do
      paths <- liftIO $ Maybe.catMaybes <$> mapM (elmExists name) srcDirs

      case (paths, Map.lookup name exposed) of
        ([path], Nothing) ->
            return (Local path)

        ([], Just [pkg]) ->
            return (Foreign pkg)

        ([], Nothing) ->
            Task.throw $ E.ModuleNotFound origin name

        (_, maybePkgs) ->
            Task.throw $ E.ModuleAmbiguous origin name paths (maybe [] id maybePkgs)


elmExists :: Module.Raw -> FilePath -> IO (Maybe FilePath)
elmExists name srcDir =
  do  let path = srcDir </> Module.nameToSlashPath name <.> "elm"
      exists <- Dir.doesFileExist path
      return $ if exists then Just path else Nothing



-- FIND KERNEL


findKernel :: FilePath -> E.Origin -> Module.Raw -> Task.Task_ E.Problem Asset
findKernel srcDir origin name =
  do  let clientPath = srcDir </> Module.nameToSlashPath name <.> "js"
      let serverPath = srcDir </> Module.nameToSlashPath name <.> "server.js"
      client <- liftIO $ Dir.doesFileExist clientPath
      server <- liftIO $ Dir.doesFileExist serverPath
      if client
        then return $ Kernel clientPath (if server then Just serverPath else Nothing)
        else Task.throw $ E.ModuleNotFound origin name
