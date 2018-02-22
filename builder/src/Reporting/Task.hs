{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Task
  ( Task, Task_, run, throw, mapError
  , Env
  , getPackageCacheDir
  , getPackageCacheDirFor
  , report
  , getReporter
  , withApproval
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
import Control.Monad.Reader (ReaderT, runReaderT, ask, asks)
import Control.Monad.Trans (liftIO)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import qualified System.IO as IO

import Elm.Package (Name, Version)
import qualified Elm.Package as Pkg
import qualified Elm.PerUserCache as PerUserCache
import qualified Reporting.Error as Error
import qualified Reporting.Progress as Progress



-- TASKS


type Task =
  Task_ Error.Error


type Task_ e =
  ExceptT e (ReaderT Env IO)


data Env =
  Env
    { _cacheDir :: FilePath
    , _worker :: IO () -> IO ()
    , _httpManager :: Http.Manager
    , _tell :: Progress.Progress -> IO ()
    }


run :: Progress.Reporter -> Task a -> IO (Maybe a)
run (Progress.Reporter tell end) task =
  do  root <- PerUserCache.getPackageRoot
      pool <- initPool 4
      httpManager <- Http.newManager Http.tlsManagerSettings
      let env = Env root pool httpManager tell
      result <- runReaderT (runExceptT task) env
      case result of
        Left err ->
          do  end (Just err)
              return Nothing

        Right answer ->
          do  end Nothing
              return (Just answer)


throw :: e -> Task_ e a
throw =
  throwError


mapError :: (x -> y) -> Task_ x a -> Task_ y a
mapError =
  withExceptT



-- CACHING


getPackageCacheDir :: Task_ e FilePath
getPackageCacheDir =
  asks _cacheDir


getPackageCacheDirFor :: Name -> Version -> Task_ e FilePath
getPackageCacheDirFor name version =
  do  cacheDir <- getPackageCacheDir
      let dir = cacheDir </> Pkg.toFilePath name </> Pkg.versionToString version
      liftIO (createDirectoryIfMissing True dir)
      return dir



-- REPORTER


report :: Progress.Progress -> Task_ e ()
report progress =
  do  tell <- asks _tell
      liftIO (tell progress)


getReporter :: Task (Progress.Progress -> IO ())
getReporter =
  asks _tell



-- APPROVAL


withApproval :: Task_ e () -> Task_ e ()
withApproval task =
  do  approval <- getApproval
      if approval then task else return ()


getApproval :: Task_ e Bool
getApproval =
  do  liftIO $ IO.hFlush IO.stdout
      input <- liftIO getLine
      case input of
        ""  -> return True
        "Y" -> return True
        "y" -> return True
        "n" -> return False
        _   ->
          do  liftIO $ putStr "Must type 'y' for yes or 'n' for no: "
              getApproval



-- RUNNER


silently :: Task_ e a -> Task_ e a
silently task =
  do  runner <- getSilentRunner
      result <- liftIO (runner task)
      either throw return result


getSilentRunner :: Task_ x (Task_ e a -> IO (Either e a))
getSilentRunner =
  do  env <- ask
      let silentEnv = env { _tell = \_ -> return () }
      return $ \task -> runReaderT (runExceptT task) silentEnv



-- THREAD POOL


workerChan :: Chan (Either e a) -> Task_ e a -> Task_ x ()
workerChan chan task =
  do  env <- ask

      liftIO $ _worker env $
        do  result <- runReaderT (runExceptT task) env
            writeChan chan result


initPool :: Int -> IO (IO () -> IO ())
initPool size =
  do  chan <- newChan

      replicateM_ size $ forkIO $ forever $
        join (readChan chan)

      return $ writeChan chan



-- HTTP


runHttp :: (Http.Manager -> (Progress.Progress -> IO ()) -> IO (Either Error.Error a)) -> Task a
runHttp fetcher =
  do  (Env _ _ manager tell) <- ask
      result <- liftIO $ fetcher manager tell
      either throwError return result
