{-# OPTIONS_GHC -Wall #-}
module Reporting.Progress.Repl
  ( create
  )
  where


import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)

import qualified Reporting.Error as Error
import qualified Reporting.Progress as Progress



-- CREATE


create :: IO Progress.Reporter
create =
  do  chan <- newChan
      mvar <- newEmptyMVar
      _ <- forkIO (loop chan >>= putMVar mvar)
      return (Progress.makeReporter chan mvar)



-- LOOP


loop :: Chan Progress.Msg -> IO ()
loop chan =
  do  msg <- readChan chan
      case msg of
        Progress.Progress _ ->
          loop chan

        Progress.End maybeError ->
          case maybeError of
            Just err ->
              Error.toStderr err

            Nothing ->
              return ()
