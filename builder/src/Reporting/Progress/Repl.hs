module Reporting.Progress.Repl
  ( reporter
  )
  where


import qualified Reporting.Exit as Exit
import qualified Reporting.Progress as Progress



-- REPORTER


reporter :: Progress.Reporter
reporter =
  Progress.Reporter (\_ -> return ()) end


end :: Maybe Exit.Exit -> IO ()
end maybeExit =
  case maybeExit of
    Just exit ->
      Exit.toStderr exit

    Nothing ->
      return ()
