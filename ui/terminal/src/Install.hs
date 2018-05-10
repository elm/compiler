module Install
  ( Args(..)
  , run
  )
  where


import qualified Elm.Install as Install
import qualified Elm.Package as Pkg
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Install as E
import qualified Reporting.Task as Task
import qualified Reporting.Progress.Terminal as Terminal



-- RUN


data Args
  = NoArgs
  | Install Pkg.Name


run :: Args -> () -> IO ()
run args () =
  do  reporter <- Terminal.create
      Task.run reporter $
        case args of
          NoArgs ->
            do  cacheDir <- Task.getPackageCacheDir
                Task.throw (Exit.Install (E.NoArgs cacheDir))

          Install pkg ->
            Install.install pkg
