module Make
  ( Flags(..)
  , ReportType(..)
  , run
  )
  where


import Control.Monad (void)

import qualified Elm.Compiler.Objects as Obj
import qualified Elm.Project as Project
import qualified Generate.Output as Output
import qualified Reporting.Task as Task
import qualified Reporting.Progress.Json as Json
import qualified Reporting.Progress.Terminal as Terminal



-- RUN


data Flags =
  Flags
    { _debug :: Bool
    , _output :: Maybe Output.Output
    , _report :: Maybe ReportType
    }


data ReportType
  = Json


run :: [FilePath] -> Flags -> IO ()
run paths (Flags debug output report) =
  let
    mode =
      if debug then Obj.Debug else Obj.Prod

    outputOptions =
      Output.Options mode Obj.Client output

    makeReporter =
      case report of
        Nothing ->
          Terminal.create

        Just Json ->
          Json.create
  in
  do  reporter <- makeReporter
      void $ Task.run reporter $
        do  summary <- Project.getRoot
            Project.compile outputOptions summary paths
