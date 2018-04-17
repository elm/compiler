module Install (run) where


import qualified Elm.Install as Install
import qualified Elm.Package as Pkg
import qualified Reporting.Task as Task
import qualified Reporting.Progress.Terminal as Terminal



-- RUN


run :: Pkg.Name -> () -> IO ()
run pkg () =
  do  reporter <- Terminal.create
      Task.run reporter $ Install.install pkg
