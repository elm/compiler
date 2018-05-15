{-# LANGUAGE OverloadedStrings #-}
module Reporting.Task
  ( Task, Task_
  , try
  , run
  , throw
  , mapError
  , Env
  , getPackageCacheDir
  , getPackageCacheDirFor
  , report
  , getReporter
  , getApproval
  , silently
  , getSilentRunner
  , workerChan
  , runHttp
  )
  where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (forever, join, replicateM_)
import Control.Monad.Except (ExceptT, runExceptT, throwError, withExceptT)
import qualified Control.Monad.Reader as R
import Control.Monad.Trans (liftIO)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import System.FilePath ((</>))

import Elm.Package (Name, Version)
import qualified Elm.Package as Pkg
import qualified Elm.PerUserCache as PerUserCache
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit
import qualified Reporting.Progress as Progress



-- TASKS


type Task =
  Task_ Exit.Exit


type Task_ e =
  ExceptT e (R.ReaderT Env IO)


data Env =
  Env
    { _cacheDir :: FilePath
    , _worker :: IO () -> IO ()
    , _httpManager :: Http.Manager
    , _tell :: Progress.Progress -> IO ()
    , _ask :: D.Doc -> IO Bool
    }


try :: Progress.Reporter -> Task a -> IO (Maybe a)
try (Progress.Reporter tell ask end) task =
  do  root <- PerUserCache.getPackageRoot
      pool <- initPool 4
      httpManager <- Http.newManager Http.tlsManagerSettings
      let env = Env root pool httpManager tell ask
      result <- R.runReaderT (runExceptT task) env
      case result of
        Left err ->
          do  end (Just err)
              return Nothing

        Right answer ->
          do  end Nothing
              return (Just answer)


run :: Progress.Reporter -> Task a -> IO ()
run reporter task =
  do  maybeAnswer <- try reporter task
      case maybeAnswer of
        Nothing -> Exit.exitFailure
        Just _ -> return ()


throw :: e -> Task_ e a
throw =
  throwError


mapError :: (x -> y) -> Task_ x a -> Task_ y a
mapError =
  withExceptT



-- CACHING


getPackageCacheDir :: Task_ e FilePath
getPackageCacheDir =
  R.asks _cacheDir


getPackageCacheDirFor :: Name -> Version -> Task_ e FilePath
getPackageCacheDirFor name version =
  do  cacheDir <- getPackageCacheDir
      let dir = cacheDir </> Pkg.toFilePath name </> Pkg.versionToString version
      liftIO (Dir.createDirectoryIfMissing True dir)
      return dir



-- REPORTER


report :: Progress.Progress -> Task_ e ()
report progress =
  do  tell <- R.asks _tell
      liftIO (tell progress)


getReporter :: Task (Progress.Progress -> IO ())
getReporter =
  R.asks _tell



-- APPROVAL


getApproval :: D.Doc -> Task_ e Bool
getApproval doc =
  do  ask <- R.asks _ask
      liftIO (ask doc)



-- RUNNER


silently :: Task_ e a -> Task_ e a
silently task =
  do  runner <- getSilentRunner
      result <- liftIO (runner task)
      either throw return result


getSilentRunner :: Task_ x (Task_ e a -> IO (Either e a))
getSilentRunner =
  do  env <- R.ask
      let silentEnv = env { _tell = \_ -> return () }
      return $ \task -> R.runReaderT (runExceptT task) silentEnv



-- THREAD POOL


workerChan :: Chan (Either e a) -> Task_ e a -> Task_ x ()
workerChan chan task =
  do  env <- R.ask

      liftIO $ _worker env $
        do  result <- R.runReaderT (runExceptT task) env
            writeChan chan result


initPool :: Int -> IO (IO () -> IO ())
initPool size =
  do  chan <- newChan

      replicateM_ size $ forkIO $ forever $
        join (readChan chan)

      return $ writeChan chan



-- HTTP


runHttp :: (Http.Manager -> (Progress.Progress -> IO ()) -> IO (Either Exit.Exit a)) -> Task a
runHttp fetcher =
  do  (Env _ _ manager tell _) <- R.ask
      result <- liftIO $ fetcher manager tell
      either throwError return result
