module Install
  ( Args(..)
  , run
  )
  where


import Control.Monad.Trans (liftIO)

import qualified Elm.Install as Install
import qualified Elm.PerUserCache as PerUserCache
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
            do  elmHome <- liftIO PerUserCache.getElmHome
                Task.throw (Exit.Install (E.NoArgs elmHome))

          Install pkg ->
            Install.install pkg
