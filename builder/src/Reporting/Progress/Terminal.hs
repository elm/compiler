{-# LANGUAGE OverloadedStrings #-}
module Reporting.Progress.Terminal
  ( create
  , newPackageOverview
  )
  where


import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import qualified System.Info as Info
import System.IO (hFlush, hPutStr, stdout)
import qualified Elm.Package as Pkg
import Elm.Package (Name, Version)

import qualified Deps.Diff as Diff
import Reporting.Doc ((<>), (<+>))
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Help as Help
import Reporting.Progress (Msg(..), Progress(..), Outcome(..), PublishPhase(..), BumpPhase(..))
import qualified Reporting.Progress as Progress
import qualified Reporting.Progress.Bar as Bar



-- CREATE


create :: IO Progress.Reporter
create =
  do  chan <- newChan
      mvar <- newEmptyMVar
      _ <- forkIO (loop chan (State 0 0 0) >>= putMVar mvar)
      return (Progress.makeReporter chan mvar)



-- STATE


data State =
  State
    { _total :: !Int
    , _good :: !Int
    , _bad :: !Int
    }



-- LOOP


loop :: Chan Msg -> State -> IO ()
loop chan state =
  do  msg <- readChan chan
      case msg of
        End (Just exit) ->
          Exit.toStderr exit

        End Nothing ->
          return ()

        Ask doc mvar ->
          do  Help.toStdout doc
              putMVar mvar =<< getApproval
              loop chan state

        Progress progress ->
          loopHelp chan progress state


getApproval :: IO Bool
getApproval =
  do  hFlush stdout
      input <- getLine
      case input of
        ""  -> return True
        "Y" -> return True
        "y" -> return True
        "n" -> return False
        _   ->
          do  putStr "Must type 'y' for yes or 'n' for no: "
              getApproval


loopHelp :: Chan Msg -> Progress -> State -> IO ()
loopHelp chan progress state@(State total good bad) =
  case progress of


    -- DOWNLOADS

    DownloadSkip ->
      do  putStrLn "Dependencies loaded from local cache."
          loop chan state

    DownloadStart [] ->
      loop chan state

    DownloadStart _ ->
      do  putStrLn "Starting downloads...\n"
          loop chan state

    DownloadPkgStart _name _version ->
      loop chan state

    DownloadPkgEnd name version outcome ->
      do  Help.toStdout (makeBullet name version outcome)
          loop chan state

    DownloadEnd Bad ->
      do  putStrLn ""
          loop chan state

    DownloadEnd Good ->
      do  putStrLn ""
          loop chan state


    -- BUILD DEPS

    BuildDepsStart size ->
      do  hPutStr stdout "Verifying dependencies..."
          hFlush stdout
          loop chan (State size 0 0)

    BuildDepsProgress ->
      do  let n = good + 1
          let msg = "\rBuilding dependencies (" ++ show n ++ "/" ++ show total ++ ")"
          hPutStr stdout msg
          hFlush stdout
          loop chan (State total n 0)

    BuildDepsEnd ->
      do  putStrLn "\rDependencies ready!                "
          loop chan (State 0 0 0)


    -- COMPILE

    CompileStart size ->
      loop chan (State size 0 0)

    CompileFileStart _ ->
      loop chan state

    CompileFileEnd _ Good ->
      do  hPutStr stdout $ Bar.render (good + 1) bad total
          hFlush stdout
          loop chan (State total (good + 1) bad)

    CompileFileEnd _ Bad ->
      do  hPutStr stdout $ Bar.render good (bad + 1) total
          hFlush stdout
          loop chan (State total good (bad + 1))

    CompileEnd ->
      let
        message =
          case (bad, total) of
            (0, 0) -> "Success!"
            (0, 1) -> "Success! Compiled 1 module."
            (0, n) -> "Success! Compiled " ++ show n ++ " modules."
            (1, _) -> "Detected errors in 1 module."
            (n, _) -> "Detected errors in " ++ show n ++ " modules."
      in
        do  hPutStr stdout $ Bar.clear
            putStrLn message
            loop chan (State 0 0 0)


    -- PUBLISH

    PublishStart name version maybePublishedVersions ->
      case maybePublishedVersions of
        Nothing ->
          do  putStrLn newPackageOverview
              putStrLn "I will now verify that everything is in order..."
              putStrLn ""
              loop chan state

        Just _ ->
          do  putStrLn $ unwords [ "Verifying", Pkg.toString name, Pkg.versionToString version, "..."  ]
              putStrLn ""
              loop chan state

    PublishCheckBump version bumpPhase ->
      do  Help.toStdout $ bumpPhaseToChecklistDoc version bumpPhase
          hFlush stdout
          loop chan state

    PublishProgress phase status ->
      do  Help.toStdout $ toChecklistDoc status (toChecklistMessages phase)
          hFlush stdout
          loop chan state

    PublishEnd ->
      do  putStrLn "Success!"
          loop chan state


    -- SOLVER

    UnableToLoadLatestPackages ->
      do  putStrLn ""
          Help.toStdout $ D.dullyellow "WARNING:" <+> "I normally check <https://package.elm-lang.org> for new packages\n"
          putStrLn "here, but my request failed. Are you offline? I will try to continue anyway.\n"
          loop chan state



-- BULLETS


makeBullet :: Name -> Version -> Outcome -> D.Doc
makeBullet name version outcome =
  let
    nm = D.fromText (Pkg.toText name)
    vsn = D.fromText (Pkg.versionToText version)

    bullet =
      case outcome of
        Good -> goodMark
        Bad -> badMark
  in
    D.indent 2 $ bullet <+> nm <+> vsn <> "\n"


goodMark :: D.Doc
goodMark =
  D.green $
    if Info.os == "mingw32" then "+" else "●"


badMark :: D.Doc
badMark =
  D.red $
    if Info.os == "mingw32" then "X" else "✗"


waitingMark :: D.Doc
waitingMark =
  D.dullyellow $
    if Info.os == "mingw32" then "-" else "→"



-- OVERVIEW OF VERSIONING


newPackageOverview :: String
newPackageOverview =
  unlines
    [ "This package has never been published before. Here's how things work:"
    , ""
    , "  - Versions all have exactly three parts: MAJOR.MINOR.PATCH"
    , ""
    , "  - All packages start with initial version " ++ Pkg.versionToString Pkg.initialVersion
    , ""
    , "  - Versions are incremented based on how the API changes:"
    , ""
    , "        PATCH = the API is the same, no risk of breaking code"
    , "        MINOR = values have been added, existing values are unchanged"
    , "        MAJOR = existing values have been changed or removed"
    , ""
    , "  - I will bump versions for you, automatically enforcing these rules"
    , ""
    ]



-- CHECKLIST


toChecklistDoc :: Maybe Outcome -> ChecklistMessages -> D.Doc
toChecklistDoc status (ChecklistMessages waiting success failure) =
  let
    padded message =
      message ++ replicate (length waiting - length message) ' '
  in
    case status of
      Nothing ->
        "  " <> waitingMark <+> D.fromString waiting

      Just Good ->
        "\r  " <> goodMark <+> D.fromString (padded success ++ "\n")

      Just Bad ->
        "\r  " <> badMark <+> D.fromString (padded failure ++ "\n\n")


data ChecklistMessages =
  ChecklistMessages
    { _waiting :: String
    , _success :: String
    , _failure :: String
    }


toChecklistMessages :: PublishPhase -> ChecklistMessages
toChecklistMessages phase =
  case phase of
    CheckReadme ->
      ChecklistMessages
        "Looking for README.md"
        "Found README.md"
        "Problem with your README.md"

    CheckLicense ->
      ChecklistMessages
        "Looking for LICENSE"
        "Found LICENSE"
        "Problem with your LICENSE"

    CheckTag version ->
      ChecklistMessages
        ("Is version " ++ Pkg.versionToString version ++ " tagged on GitHub?")
        ("Version " ++ Pkg.versionToString version ++ " is tagged on GitHub")
        ("Version " ++ Pkg.versionToString version ++ " is not tagged on GitHub!")

    CheckDownload ->
      ChecklistMessages
        "Downloading code from GitHub..."
        "Code downloaded successfully from GitHub"
        "Could not download code from GitHub!"

    CheckBuild ->
      ChecklistMessages
        "Building downloaded code and generated docs..."
        "Downloaded code compiles successfully / docs generated\n"
        "Cannot compile downloaded code!"

    CheckChanges ->
      ChecklistMessages
        "Checking for uncommitted changes..."
        "No uncommitted changes in local code"
        "Your local code is different than the code tagged on GitHub"



-- BUMP PHASE


bumpPhaseToChecklistDoc :: Pkg.Version -> BumpPhase -> D.Doc
bumpPhaseToChecklistDoc version bumpPhase =
  let
    mkMsgs success =
      ChecklistMessages
        ("Checking semantic versioning rules. Is " ++ Pkg.versionToString version ++ " correct?")
        success
        ("Version " ++ Pkg.versionToString version ++ " is not correct!")
  in

  case bumpPhase of
    StatedVersion ->
      toChecklistDoc Nothing $ mkMsgs ""

    GoodStart ->
      toChecklistDoc (Just Good) $ mkMsgs $
        "All packages start at version " ++ Pkg.versionToString Pkg.initialVersion

    GoodBump old magnitude ->
      toChecklistDoc (Just Good) $ mkMsgs $
        toCheckVersionSuccessMessage old version magnitude

    BadBump ->
      toChecklistDoc (Just Bad) $ mkMsgs ""


toCheckVersionSuccessMessage :: Pkg.Version -> Pkg.Version -> Diff.Magnitude -> String
toCheckVersionSuccessMessage oldVersion newVersion magnitude =
  let
    old = Pkg.versionToString oldVersion
    new = Pkg.versionToString newVersion
    mag = Diff.magnitudeToString magnitude
  in
    "Version number " ++ new ++ " verified (" ++ mag ++ " change, " ++ old ++ " => " ++ new ++ ")"
