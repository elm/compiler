module Reporting.Progress.Json
  ( reporter
  )
  where


import qualified Data.ByteString.Builder as B
import System.IO (stderr)

import qualified Reporting.Exit as Exit
import qualified Reporting.Progress as Progress
import qualified Json.Encode as Encode



-- REPORTER


reporter :: Progress.Reporter
reporter =
  Progress.Reporter
    (\_ -> return ())
    (\_ -> return True)
    end


end :: Maybe Exit.Exit -> IO ()
end maybeExit =
  case maybeExit of
    Just exit ->
      B.hPutBuilder stderr (Encode.encodeUgly (Exit.toJson exit))

    Nothing ->
      return ()
