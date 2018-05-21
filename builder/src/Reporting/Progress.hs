{-# OPTIONS_GHC -Wall #-}
module Reporting.Progress
  ( Reporter(..)
  , makeReporter
  , silentReporter
  , Msg(..)
  , Progress(..)
  , Outcome(..)
  , PublishPhase(..)
  , BumpPhase(..)
  )
  where


import Control.Concurrent.Chan (Chan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar)
import qualified Elm.Compiler.Module as Module
import Deps.Diff (Magnitude)
import Elm.Package (Name, Version)
import qualified Reporting.Doc as D
import Reporting.Exit (Exit)



-- REPORTER


data Reporter =
  Reporter
    { _tell :: Progress -> IO ()
    , _ask :: D.Doc -> IO Bool
    , _end :: Maybe Exit -> IO ()
    }


makeReporter :: Chan Msg -> MVar () -> Reporter
makeReporter chan mvar =
  let
    tell progress =
      writeChan chan (Progress progress)

    ask doc =
      do  var <- newEmptyMVar
          writeChan chan (Ask doc var)
          readMVar var

    end maybeError =
      do  writeChan chan (End maybeError)
          readMVar mvar
  in
  Reporter tell ask end


silentReporter :: Reporter
silentReporter =
  Reporter
    (\_ -> return ())
    (\_ -> return True)
    (\_ -> return ())



-- MESSAGES


data Msg
  = Progress Progress
  | Ask D.Doc (MVar Bool)
  | End (Maybe Exit)


data Progress
  -- download packages
  = DownloadSkip
  | DownloadStart [(Name, Version)]
  | DownloadPkgStart Name Version
  | DownloadPkgEnd Name Version Outcome
  | DownloadEnd Outcome

  -- build dependencies
  | BuildDepsStart Int
  | BuildDepsProgress
  | BuildDepsEnd

  -- compile files
  | CompileStart Int
  | CompileFileStart Module.Raw
  | CompileFileEnd Module.Raw Outcome
  | CompileEnd

  -- publish
  | PublishStart Name Version (Maybe [Version])
  | PublishCheckBump Version BumpPhase
  | PublishProgress PublishPhase (Maybe Outcome)
  | PublishEnd

  -- solver
  | UnableToLoadLatestPackages


data Outcome = Good | Bad


data PublishPhase
  = CheckReadme
  | CheckLicense
  | CheckTag Version
  | CheckDownload
  | CheckBuild
  | CheckChanges


data BumpPhase
  = StatedVersion
  | GoodStart
  | GoodBump Version Magnitude
  | BadBump
