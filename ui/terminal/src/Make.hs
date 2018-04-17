module Make
  ( Flags(..)
  , run
  , ReportType(..)
  , reportType
  , docsFile
  )
  where


import qualified System.FilePath as FP

import qualified Elm.Project as Project
import qualified Generate.Output as Output
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Make as E
import qualified Reporting.Task as Task
import qualified Reporting.Progress as Progress
import qualified Reporting.Progress.Json as Json
import qualified Reporting.Progress.Terminal as Terminal
import Terminal.Args (Parser(..))



-- RUN


data Flags =
  Flags
    { _debug :: Bool
    , _optimize :: Bool
    , _output :: Maybe Output.Output
    , _report :: Maybe ReportType
    , _docs :: Maybe FilePath
    }


run :: [FilePath] -> Flags -> IO ()
run paths (Flags debug optimize output report docs) =
  do  reporter <- toReporter report
      Task.run reporter $
        do  mode <- toMode debug optimize
            summary <- Project.getRoot
            Project.compile mode Output.Client output docs summary paths


toMode :: Bool -> Bool -> Task.Task Output.Mode
toMode debug optimize =
  case (debug, optimize) of
    (True , True ) -> Task.throw $ Exit.Make E.CannotOptimizeAndDebug
    (False, True ) -> return Output.Prod
    (False, False) -> return Output.Dev
    (True , False) -> return Output.Debug


toReporter :: Maybe ReportType -> IO Progress.Reporter
toReporter report =
  case report of
    Nothing -> Terminal.create
    Just Json -> return Json.reporter



-- REPORT


data ReportType
  = Json


reportType :: Parser ReportType
reportType =
  Parser
    { _singular = "report type"
    , _plural = "report types"
    , _parser = \string -> if string == "json" then Just Json else Nothing
    , _suggest = \_ -> return ["json"]
    , _examples = \_ -> return ["json"]
    }



-- DOCS


docsFile :: Parser FilePath
docsFile =
  Parser
    { _singular = "json file"
    , _plural = "json files"
    , _parser = \string -> if FP.takeExtension string == ".json" then Just string else Nothing
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["docs.json","documentation.json"]
    }
