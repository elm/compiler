module Make
  ( Flags(..)
  , run
  )
  where


import Control.Monad (void)

import qualified Elm.Compiler.Objects as Obj
import qualified Elm.Project as Project
import qualified Generate.Output as Output
import qualified Reporting.Task as Task
import qualified Reporting.Progress.Terminal as Terminal



-- RUN


data Flags =
  Flags
    { _warn :: Bool
    , _debug :: Bool
    , _output :: Maybe Output.Output
    }


run :: [FilePath] -> Flags -> IO ()
run paths flags =
  do  reporter <- Terminal.create
      void $ Task.run reporter $
        do  summary <- Project.getRoot
            Project.compile (toOptions flags) summary paths


-- TODO figure out --warn flag
toOptions :: Flags -> Output.Options
toOptions (Flags warn debug output) =
  let
    mode = if debug then Obj.Debug else Obj.Prod
    target = if False then Obj.Server else Obj.Client
  in
  Output.Options warn mode target output
