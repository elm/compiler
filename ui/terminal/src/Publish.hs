{-# OPTIONS_GHC -Wall #-}
module Publish (run) where


import Control.Monad (void)

import qualified Elm.Project as Project
import qualified Elm.Publish as Publish
import qualified Reporting.Task as Task
import qualified Reporting.Progress.Terminal as Terminal



-- RUN


run :: IO ()
run =
  do  reporter <- Terminal.create
      void $ Task.run reporter $
        do  summary <- Project.getRoot
            Publish.publish summary
