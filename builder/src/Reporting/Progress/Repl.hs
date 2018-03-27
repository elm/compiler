module Reporting.Progress.Repl
  ( create
  )
  where


import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)

import qualified Reporting.Exit as Exit
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

        Progress.End maybeExit ->
          case maybeExit of
            Just exit ->
              Exit.toStderr exit

            Nothing ->
              return ()
