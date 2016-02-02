module Main where

import qualified System.Directory as Dir
import qualified System.Environment as Env
import System.Exit (exitWith)
import System.IO (hPutStrLn, stderr, stdin, stdout)
import qualified System.Process as Proc

import qualified Elm.Compiler.Version as Compiler
import qualified Elm.Package as Pkg

main :: IO ()
main =
  do  allArgs <- Env.getArgs
      case allArgs of
        [] ->
          putStrLn usageMessage

        command : args ->
          attemptToRun command args


attemptToRun :: String -> [String] -> IO ()
attemptToRun command args =
  do  found <- Dir.findExecutable ("elm-" ++ command)
      case found of
        Nothing ->
          hPutStrLn stderr $
            "Could not find command `" ++ command ++ "`. Maybe there is a typo?\n\n\
            \Default commands include:\n\n"
            ++ availableCommands ++
            "\nWhen you try to run the command `" ++ command ++ "` we actually search for an\n\
            \  executable named elm-" ++ command ++ ". Are you able to run elm-" ++ command ++ "?\n\
            \  Is it on your PATH?"

        Just path ->
          let createProc =
                (Proc.proc path args)
                  { Proc.std_in  = Proc.UseHandle stdin
                  , Proc.std_out = Proc.UseHandle stdout
                  , Proc.std_err = Proc.UseHandle stderr
                  }
          in
              do  (_, _, _, handle) <- Proc.createProcess createProc
                  exitWith =<< Proc.waitForProcess handle


usageMessage :: String
usageMessage =
  "Elm Platform " ++ (Pkg.versionToString Compiler.version) ++ " - a way to run all Elm tools\n\
  \\n\
  \Usage: elm <command> [<args>]\n\
  \\n\
  \Available commands include:\n\n"
  ++ availableCommands ++
  "\nYou can learn more about a specific command by running things like:\n\
  \\n\
  \  elm make --help\n\
  \  elm package --help\n\
  \  elm <command> --help\n\
  \\n\
  \In all these cases we are simply running 'elm-<command>' so if you create an\n\
  \executable named 'elm-foobar' you will be able to run it as 'elm foobar' as\n\
  \long as it appears on your PATH."


availableCommands :: String
availableCommands =
  "  make      Compile an Elm file or project into JS or HTML\n\
  \  package   Manage packages from <http://package.elm-lang.org>\n\
  \  reactor   Develop with compile-on-refresh and time-travel debugging\n\
  \  repl      A REPL for running individual expressions\n"

