{-# OPTIONS_GHC -Wall #-}
module File.Watcher where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import qualified System.FSNotify as Notify

import qualified Elm.Project as Project
import Elm.Project (Project)



-- GRAPH


data Graph =
  Graph
    { _elm :: Map.Map Module.Raw Node
    , _js :: Map.Map Module.Raw FilePath
    }


data Node =
  Node
    { _path :: FilePath
    , _time :: UTCTime
    , _needs :: Set.Set Module.Raw
    , _blocks :: Set.Set Module.Raw
    , _iface :: Maybe (UTCTime, Interface)
    }



-- ABC


action :: Notify.Event -> IO ()
action event =
  case event of
    Notify.Added path time ->
      return ()

    Notify.Modified path time ->
      return ()

    Notify.Removed path time ->
      return ()


watcher :: Project -> IO ()
watcher project =
  let
    srcDir =
      Project.toSourceDir project

    action event =
      case event of
        Notify.Added _ _ ->
          return ()

        Notify.Modified path time ->

        Notify.Removed path time ->

  in
    do  killer <- newChan
        mapM_ (watcherHelp killer action) [srcDir]



watcherHelp :: Notify.Action -> FilePath -> IO ()
watcherHelp killer action dir =
  void $ forkIO $ Notify.withManager $ \manager ->
    do  stop <- Notify.watchTree manager dir (const True) action
        _ <- readChan killer
        stop
