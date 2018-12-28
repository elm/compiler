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
import qualified Elm.ModuleName as ModuleName
import Deps.Diff (Magnitude)
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
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
  | DownloadStart [(Pkg.Name, V.Version)]
  | DownloadPkgStart Pkg.Name V.Version
  | DownloadPkgEnd Pkg.Name V.Version Outcome
  | DownloadEnd Outcome

  -- build dependencies
  | BuildDepsStart Int
  | BuildDepsProgress
  | BuildDepsEnd

  -- compile files
  | CompileStart Int
  | CompileFileStart ModuleName.Raw
  | CompileFileEnd ModuleName.Raw Outcome
  | CompileEnd

  -- publish
  | PublishStart Pkg.Name V.Version (Maybe [V.Version])
  | PublishCheckBump V.Version BumpPhase
  | PublishProgress PublishPhase (Maybe Outcome)
  | PublishEnd

  -- solver
  | UnableToLoadLatestPackages


data Outcome = Good | Bad


data PublishPhase
  = CheckReadme
  | CheckLicense
  | CheckTag V.Version
  | CheckDownload
  | CheckBuild
  | CheckChanges


data BumpPhase
  = StatedVersion
  | GoodStart
  | GoodBump V.Version Magnitude
  | BadBump
