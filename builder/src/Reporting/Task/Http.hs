{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Task.Http
  ( Fetch
  , Handler
  , run
  , package
  , anything
  , andThen
  , parallel
  , report
  )
  where


import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Exception (Exception, SomeException, catch)
import Control.Monad (forever, join, replicateM_, void)
import qualified Data.ByteString.Char8 as BS
import qualified Network.HTTP as Http (urlEncodeVars)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Header as Http (hAcceptEncoding, hUserAgent)

import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Http as E
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task



-- FETCH


data Fetch a where
  Package :: String -> [(String,String)] -> Handler a -> Fetch a
  Anything :: String -> Handler a -> Fetch a
  AndThen :: Fetch a -> (a -> Fetch b) -> Fetch b
  Parallel :: [Fetch a] -> Fetch [a]
  Report :: Progress.Progress -> (Progress.Outcome -> Progress.Progress) -> Fetch a -> Fetch a


type Handler a =
  Http.Request -> Http.Manager -> IO (Either E.Exit a)


package :: String -> [(String,String)] -> Handler a -> Fetch a
package =
  Package


anything :: String -> Handler a -> Fetch a
anything =
  Anything


andThen :: Fetch a -> (a -> Fetch b) -> Fetch b
andThen =
  AndThen


parallel :: [Fetch a] -> Fetch [a]
parallel =
  Parallel


report :: Progress.Progress -> (Progress.Outcome -> Progress.Progress) -> Fetch a -> Fetch a
report =
  Report



-- RUN


run :: Fetch a -> Task.Task a
run fetch =
  Task.runHttp $ \manager tell ->
    do  chan <- newChan
        replicateM_ 4 $ forkIO $ forever $ join (readChan chan)
        readMVar =<< runHelp chan manager tell fetch


runHelp :: Chan (IO ()) -> Http.Manager -> (Progress.Progress -> IO ()) -> Fetch a -> IO (MVar (Either Exit.Exit a))
runHelp chan manager tell fetch =
  case fetch of
    Package path params handler ->
      do  mvar <- newEmptyMVar
          let url = makePackageUrl path params
          writeChan chan $ putMVar mvar =<< fetchSafe url manager handler
          return mvar

    Anything url handler ->
      do  mvar <- newEmptyMVar
          writeChan chan $ putMVar mvar =<< fetchSafe url manager handler
          return mvar

    AndThen subFetch callback ->
      do  subMVar <- runHelp chan manager tell subFetch
          mvar <- newEmptyMVar
          void $ forkIO $
            do  result <- readMVar subMVar
                putMVar mvar =<<
                  case result of
                    Left err ->
                      return (Left err)
                    Right value ->
                      readMVar =<< runHelp chan manager tell (callback value)
          return mvar

    Parallel fetches ->
      do  mvars <- mapM (runHelp chan manager tell) fetches
          mvar <- newEmptyMVar
          void $ forkIO $
            do  results <- mapM readMVar mvars
                putMVar mvar (sequence results)
          return mvar

    Report start toEnd subFetch ->
      do  tell start
          subMVar <- runHelp chan manager tell subFetch
          mvar <- newEmptyMVar
          void $ forkIO $
            do  result <- readMVar subMVar
                tell $ toEnd $
                  either (\_ -> Progress.Bad) (\_ -> Progress.Good) result
                putMVar mvar result
          return mvar



-- PACKAGE URLS


packageDomain :: String
packageDomain =
  "https://package.elm-lang.org"


makePackageUrl :: String -> [(String,String)] -> String
makePackageUrl path params =
  let
    query =
      if null params then
        ""
      else
        "?" ++ Http.urlEncodeVars params
  in
    packageDomain ++ "/" ++ path ++ query



-- HTTP HELP


fetchSafe :: String -> Http.Manager -> Handler a -> IO (Either Exit.Exit a)
fetchSafe url manager handler =
  fetchUnsafe url manager handler
    `catch` \e -> handleAnyError url (e :: SomeException)


fetchUnsafe :: String -> Http.Manager -> Handler a -> IO (Either Exit.Exit a)
fetchUnsafe url manager handler =
  do  request <- Http.parseUrlThrow url
      result <- handler (addHeaders request) manager
      case result of
        Right value ->
          return (Right value)

        Left problem ->
          return (Left (Exit.BadHttp url problem))


handleAnyError :: (Exception e) => String -> e -> IO (Either Exit.Exit a)
handleAnyError url exception =
  return $ Left $ Exit.BadHttp url $ E.Unknown $ show exception


addHeaders :: Http.Request -> Http.Request
addHeaders request =
  request
    { Http.requestHeaders =
        [ (Http.hUserAgent, userAgent)
        , (Http.hAcceptEncoding, "gzip")
        ]
    }


{-# NOINLINE userAgent #-}
userAgent :: BS.ByteString
userAgent =
  BS.pack ("elm/" ++ Pkg.versionToString Compiler.version)