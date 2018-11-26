{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module File.Plan
  ( plan
  , Info(..)
  )
  where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Monad (foldM, void, when)
import Control.Monad.Except (liftIO)
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Time.Clock as Time
import qualified System.Directory as Dir

import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified File.Crawl as Crawl
import qualified File.Header as Header
import qualified Reporting.Task as Task
import qualified Stuff.Paths



-- PLAN


type Dict value = Map.Map ModuleName.Raw value


plan :: Maybe FilePath -> Summary.Summary -> Crawl.Result -> Task.Task (Dict Info, I.Interfaces)
plan docs (Summary.Summary root project _ ifaces _) (Crawl.Graph _ locals _ foreigns _) =
  liftIO $
  do  queue <- newChan
      let env = Env queue root (Project.getName project) docs

      mvar <- newEmptyMVar
      statusMVars <- Map.traverseWithKey (getStatus env mvar foreigns) locals
      putMVar mvar statusMVars

      void $ forkIO $
        do  graph <- Map.traverseMaybeWithKey (\_ -> readMVar) statusMVars
            writeChan queue (EndLoop graph)

      ifaceLoader queue ifaces


data Env =
  Env
    { _queue :: Chan Msg
    , _root :: FilePath
    , _pkg :: Pkg.Name
    , _docs :: Maybe FilePath
    }



-- STATUS


type Status = Maybe Info
  -- Nothing == clean
  -- Just info == dirty


data Info =
  Info
    { _path :: FilePath
    , _time :: Time.UTCTime
    , _src :: BS.ByteString
    , _clean :: [ModuleName.Raw]
    , _dirty :: [ModuleName.Raw]
    , _foreign :: [ModuleName.Canonical]  -- TODO is this needed?
    }



-- GET STATUS


getStatus
  :: Env
  -> MVar (Dict (MVar Status))
  -> Dict Pkg.Canonical
  -> ModuleName.Raw
  -> Header.Info
  -> IO (MVar Status)
getStatus env statusMVars foreigns name (Header.Info path time src deps) =
  do  mvar <- newEmptyMVar

      void $ forkIO $ putMVar mvar =<<
        do  statuses <- readMVar statusMVars
            info <- foldM (addDep statuses foreigns) (Info path time src [] [] []) deps

            let elmi = Stuff.Paths.elmi (_root env) name
            let docs = Stuff.Paths.moduleDocs (_root env) name

            case _dirty info of
              _ : _ ->
                do  remove elmi
                    remove docs
                    return (Just info)

              [] ->
                do  freshElmi <- isFresh time elmi
                    freshDocs <-
                      case _docs env of
                        Nothing -> return True
                        Just _ -> isFresh time docs

                    if name /= "Main" && freshElmi && freshDocs
                      then
                        do  let canonical = ModuleName.Canonical (_pkg env) name
                            writeChan (_queue env) (Get canonical elmi)
                            return Nothing
                      else
                        do  remove elmi
                            remove docs
                            return (Just info)

      return mvar


addDep :: Dict (MVar Status) -> Dict Pkg.Canonical -> Info -> ModuleName.Raw -> IO Info
addDep locals foreigns info name =
  case Map.lookup name locals of
    Just mvar ->
      do  status <- readMVar mvar
          case status of
            Nothing ->
              return $ info { _clean = name : _clean info }

            Just _ ->
              return $ info { _dirty = name : _dirty info }

    Nothing ->
      case Map.lookup name foreigns of
        Just (Pkg.Canonical pkg _vsn) ->
          return $ info { _foreign = ModuleName.Canonical pkg name : _foreign info }

        Nothing ->
          return info -- must be native



-- IS FRESH


isFresh :: Time.UTCTime -> FilePath -> IO Bool
isFresh srcTime cachedFile =
  andM
    [ Dir.doesFileExist cachedFile
    , do  cacheTime <- Dir.getModificationTime cachedFile
          return (cacheTime >= srcTime)
    ]


andM :: [IO Bool] -> IO Bool
andM checks =
  case checks of
    [] ->
      return True

    check : otherChecks ->
      do  bool <- check
          if bool then andM otherChecks else return False


remove :: FilePath -> IO ()
remove path =
  do  exists <- Dir.doesFileExist path
      when exists (Dir.removeFile path)



-- INTERFACE LOADER


data Msg
  = EndLoop (Dict Info)
  | Get ModuleName.Canonical FilePath


ifaceLoader :: Chan Msg -> I.Interfaces -> IO (Dict Info, I.Interfaces)
ifaceLoader queue ifaces =
  do  msg <- readChan queue
      case msg of
        EndLoop dirty ->
          return ( dirty, ifaces )

        Get canonical elmi ->
          case Map.lookup canonical ifaces of
            Just _ ->
              ifaceLoader queue ifaces

            Nothing ->
              do  iface <- Binary.decodeFile elmi
                  ifaceLoader queue $ Map.insert canonical iface ifaces
