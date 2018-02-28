{-# OPTIONS_GHC -Wall #-}
module Diff (run) where


import Control.Monad (void)

import qualified Elm.Diff as Diff
import qualified Reporting.Task as Task
import qualified Reporting.Progress.Terminal as Terminal



-- RUN


run :: Diff.Args -> IO ()
run args =
  do  reporter <- Terminal.create
      void $ Task.run reporter $ Diff.diff args
