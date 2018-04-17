{-# OPTIONS_GHC -Wall #-}
module Diff (run) where


import qualified Elm.Diff as Diff
import qualified Reporting.Task as Task
import qualified Reporting.Progress.Terminal as Terminal



-- RUN


run :: Diff.Args -> () -> IO ()
run args () =
  do  reporter <- Terminal.create
      Task.run reporter $ Diff.diff args
