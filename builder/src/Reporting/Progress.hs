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
import Control.Concurrent.MVar (MVar, readMVar)
import qualified Elm.Compiler.Module as Module
import Deps.Diff (Magnitude)
import Elm.Package (Name, Version)
import Reporting.Exit (Exit)



-- REPORTER


data Reporter =
  Reporter
    { _tell :: Progress -> IO ()
    , _end :: Maybe Exit -> IO ()
    }


makeReporter :: Chan Msg -> MVar () -> Reporter
makeReporter chan mvar =
  let
    tell progress =
      writeChan chan (Progress progress)

    end maybeError =
      do  writeChan chan (End maybeError)
          readMVar mvar
  in
    Reporter tell end


silentReporter :: Reporter
silentReporter =
  Reporter
    (\_ -> return ())
    (\_ -> return ())



-- MESSAGES


data Msg = Progress Progress | End (Maybe Exit)


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
