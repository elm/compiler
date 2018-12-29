module File.Compile
  ( compile
  , Answer(..)
  )
  where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Monad (void)
import Control.Monad.Except (liftIO)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Time.Clock as Time

import qualified Elm.Compiler as Compiler
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import Elm.Project.Json (Project)
import qualified Elm.Project.Json as Project
import qualified File.Plan as Plan
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task



-- COMPILE


compile :: Project -> I.Interfaces -> Dict Plan.Info -> Task.Task (Dict Answer)
compile project ifaces modules =
  do  Task.report (Progress.CompileStart (Map.size modules))

      tell <- Task.getReporter

      answers <- liftIO $
        do  mvar <- newEmptyMVar
            iMVar <- newMVar ifaces
            answerMVars <- Map.traverseWithKey (compileModule tell project mvar iMVar) modules
            putMVar mvar answerMVars
            traverse readMVar answerMVars

      Task.report Progress.CompileEnd

      return answers



-- ANSWERS


data Answer
  = Blocked
  | Bad FilePath Time.UTCTime BS.ByteString [Compiler.Error]
  | Good Compiler.Artifacts


type Dict a = Map.Map ModuleName.Raw a



-- COMPILE MODULE


compileModule
  :: (Progress.Progress -> IO ())
  -> Project
  -> MVar (Dict (MVar Answer))
  -> MVar I.Interfaces
  -> ModuleName.Raw
  -> Plan.Info
  -> IO (MVar Answer)
compileModule tell project answersMVar ifacesMVar name info =
  do  mvar <- newEmptyMVar

      void $ forkIO $
        do  answers <- readMVar answersMVar
            blocked <- isBlocked answers info
            if blocked
              then putMVar mvar Blocked
              else
                do  tell (Progress.CompileFileStart name)
                    let pkg = Project.getName project
                    let imports = makeImports project info
                    ifaces <- readMVar ifacesMVar
                    let source = Plan._src info
                    case Compiler.compile pkg imports ifaces source of
                      (_warnings, Left errors) ->
                        do  tell (Progress.CompileFileEnd name Progress.Bad)
                            let time = Plan._time info
                            let path = Plan._path info
                            putMVar mvar (Bad path time source errors)

                      (_warnings, Right result@(Compiler.Artifacts elmi _)) ->
                        do  tell (Progress.CompileFileEnd name Progress.Good)
                            let canonicalName = ModuleName.Canonical pkg name
                            lock <- takeMVar ifacesMVar
                            putMVar ifacesMVar (Map.insert canonicalName elmi lock)
                            putMVar mvar (Good result)

      return mvar



-- IMPORTS


makeImports :: Project -> Plan.Info -> Dict ModuleName.Canonical
makeImports project (Plan.Info _ _ _ clean dirty foreign) =
  let
    pkgName =
      Project.getName project

    mkLocal name =
      ( name, ModuleName.Canonical pkgName name )

    mkForeign canonicalName@(ModuleName.Canonical _ name) =
      ( name, canonicalName )
  in
    Map.fromList $
      map mkLocal clean
      ++ map mkLocal dirty
      ++ map mkForeign foreign



-- INTERFACES


isBlocked :: Dict (MVar Answer) -> Plan.Info -> IO Bool
isBlocked answers info =
  anyBlock <$> traverse (get answers) (Plan._dirty info)


get :: Dict (MVar Answer) -> ModuleName.Raw -> IO Answer
get names name =
  case Map.lookup name names of
    Nothing ->
      error "bug manifesting in File.Complie.get, please report at <TODO>!"

    Just mvar ->
      readMVar mvar


anyBlock :: [Answer] -> Bool
anyBlock answers =
  case answers of
    [] ->
      False

    Blocked : _ ->
      True

    Bad _ _ _ _ : _ ->
      True

    Good _ : otherAnswers ->
      anyBlock otherAnswers
