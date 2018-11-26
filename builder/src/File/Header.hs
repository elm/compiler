{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module File.Header
  ( Info(..)
  , readModule
  , readOneFile
  , readManyFiles
  , readSource
  )
  where

import Control.Monad.Except (liftIO)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Time.Calendar as Day
import qualified Data.Time.Clock as Time
import qualified System.Directory as Dir

import qualified Elm.Header as Header
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Project.Json as Project
import Elm.Project.Json (Project)
import Elm.Project.Summary (Summary(..))
import qualified File.IO as IO
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Crawl as E
import qualified Reporting.Task as Task



-- INFO


data Info =
  Info
    { _path :: FilePath
    , _time :: Time.UTCTime
    , _source :: BS.ByteString
    , _imports :: [ModuleName.Raw]
    }


atRoot :: Task.Task_ E.Problem a -> Task.Task_ E.Exit a
atRoot task =
  Task.mapError (\problem -> E.DependencyProblems problem []) task


-- READ MODULE


readModule :: Summary -> ModuleName.Raw -> FilePath -> Task.Task_ E.Problem (ModuleName.Raw, Info)
readModule summary expectedName path =
  do  time <- liftIO $ Dir.getModificationTime path
      source <- liftIO $ IO.readUtf8 path
      (maybeName, info) <- parse (_project summary) path time source
      name <- checkName path expectedName maybeName
      return (name, info)


checkName :: FilePath -> ModuleName.Raw -> Maybe ModuleName.Raw -> Task.Task_ E.Problem ModuleName.Raw
checkName path expectedName maybeName =
  case maybeName of
    Nothing ->
      Task.throw (E.ModuleNameMissing path expectedName)

    Just actualName ->
      if expectedName == actualName
        then return expectedName
        else Task.throw (E.ModuleNameMismatch path expectedName actualName)



-- READ ONE FILE


readOneFile :: Summary -> FilePath -> Task.Task (Maybe ModuleName.Raw, Info)
readOneFile summary path =
  Task.mapError Exit.Crawl $
  do  (time, source) <- readOneHelp path
      atRoot $ parse (_project summary) path time source


readOneHelp :: FilePath -> Task.Task_ E.Exit (Time.UTCTime, BS.ByteString)
readOneHelp path =
  do  exists <- IO.exists path
      if exists
        then liftIO $ (,) <$> Dir.getModificationTime path <*> IO.readUtf8 path
        else Task.throw $ E.RootFileNotFound path



-- READ MANY FILES


readManyFiles :: Summary -> FilePath -> [FilePath] -> Task.Task ((ModuleName.Raw, Info), [(ModuleName.Raw, Info)])
readManyFiles summary file files =
  Task.mapError Exit.Crawl $
  do  info <- readManyFilesHelp summary file
      infos <- traverse (readManyFilesHelp summary) files
      let nameTable = foldr insert Map.empty (info:infos)
      _ <- Map.traverseWithKey detectDuplicateNames nameTable
      return (info, infos)
  where
    append (x,xs) (y,ys) = (x, xs ++ y : ys)
    insert (k,v) dict = Map.insertWith append k (v, []) dict


readManyFilesHelp :: Summary -> FilePath -> Task.Task_ E.Exit (ModuleName.Raw, Info)
readManyFilesHelp summary path =
  do  (time, source) <- readOneHelp path
      (maybeName, info) <- atRoot $ parse (_project summary) path time source
      case maybeName of
        Nothing ->
          Task.throw (E.RootNameless path)

        Just name ->
          return (name, info)


detectDuplicateNames :: ModuleName.Raw -> (Info, [Info]) -> Task.Task_ E.Exit ()
detectDuplicateNames name (info, otherInfos) =
  case otherInfos of
    [] ->
      return ()

    _ ->
      Task.throw (E.RootModuleNameDuplicate name (map _path (info : otherInfos)))




-- READ SOURCE


readSource :: Project -> BS.ByteString -> Task.Task (Maybe ModuleName.Raw, Info)
readSource project source =
  Task.mapError Exit.Crawl $
    atRoot $ parse project "elm" fakeTime source


fakeTime :: Time.UTCTime
fakeTime =
  Time.UTCTime (Day.fromGregorian 3000 1 1) 0



-- PARSE HEADER


parse :: Project -> FilePath -> Time.UTCTime -> BS.ByteString -> Task.Task_ E.Problem (Maybe ModuleName.Raw, Info)
parse project path time source =
  -- TODO get regions on data extracted here
  case Header.parse (Project.getName project) source of
    Right (maybeDecl, deps) ->
      do  maybeName <- checkTag project path maybeDecl
          return ( maybeName, Info path time source deps )

    Left msg ->
      Task.throw (E.BadHeader path time source msg)


checkTag :: Project -> FilePath -> Maybe (Header.Tag, ModuleName.Raw) -> Task.Task_ E.Problem (Maybe ModuleName.Raw)
checkTag project path maybeDecl =
  case maybeDecl of
    Nothing ->
      return Nothing

    Just (tag, name) ->
      let
        success =
          return (Just name)
      in
      case tag of
        Header.Normal ->
          success

        Header.Port ->
          case project of
            Project.App _ ->
              success

            Project.Pkg _ ->
              Task.throw (E.PortsInPackage path name)

        Header.Effect ->
          if Project.isPlatformPackage project then
            success

          else
            Task.throw (E.EffectsUnexpected path name)
