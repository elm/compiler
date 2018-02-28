{-# OPTIONS_GHC -Wall #-}
module Bump (run) where


import Control.Monad (void)

import qualified Elm.Bump as Bump
import qualified Elm.Project as Project
import qualified Reporting.Task as Task
import qualified Reporting.Progress.Terminal as Terminal



-- RUN


run :: IO ()
run =
  do  reporter <- Terminal.create
      void $ Task.run reporter $
        do  summary <- Project.getRoot
            Bump.bump summary
