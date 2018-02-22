{-# OPTIONS_GHC -Wall #-}
module Install (run) where


import Control.Monad (void)

import qualified Elm.Install as Install
import qualified Elm.Package as Pkg
import qualified Reporting.Task as Task
import qualified Reporting.Progress.Terminal as Terminal



-- RUN


run :: Pkg.Name -> IO ()
run pkg =
  do  reporter <- Terminal.create
      void $ Task.run reporter $
        Install.install pkg
