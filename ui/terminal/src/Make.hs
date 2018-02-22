{-# OPTIONS_GHC -Wall #-}
module Make (run) where


import Control.Monad (void)

import qualified Elm.Compiler.Objects as Obj
import qualified Elm.Project as Project
import qualified Elm.Project.Flags as Flags
import qualified Reporting.Task as Task
import qualified Reporting.Progress.Terminal as Terminal

import qualified CommandLine.Args as Args



-- RUN


run :: [FilePath] -> Args.MakeFlags -> IO ()
run paths flags =
  do  reporter <- Terminal.create
      void $ Task.run reporter $
        do  summary <- Project.getRoot
            Project.compile (toOptions flags) summary paths


-- TODO figure out --warn flag
toOptions :: Args.MakeFlags -> Flags.Options
toOptions (Args.MakeFlags _warn debug output server) =
  let
    mode = if debug then Obj.Debug else Obj.Prod
    target = if server then Obj.Server else Obj.Client
  in
  Flags.Options mode target output
