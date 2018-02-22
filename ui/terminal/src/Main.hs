{-# OPTIONS_GHC -Wall #-}
module Main where

import qualified System.Directory as Dir
import System.Exit (exitWith)
import qualified System.IO as IO
import qualified System.Process as Proc
import GHC.IO.Encoding (setLocaleEncoding, utf8)

import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg

import CommandLine.Args as Args
import qualified Bump
import qualified Develop
import qualified Diff
import qualified Install
import qualified Make
import qualified Publish
import qualified Repl



-- MAIN


main :: IO ()
main =
  do  setLocaleEncoding utf8

      route <- Args.chomp
      case route of
        Naked () () ->
          putStrLn $
            "Elm " ++ Pkg.versionToString Compiler.version
            ++ " - Get started at <https://guide.elm-lang.org/>!"

        Version () () ->
          putStrLn (Pkg.versionToString Compiler.version)

        Install pkg () ->
          Install.run pkg

        Publish () () ->
          Publish.run

        Make paths flags ->
          Make.run paths flags

        Bump () () ->
          Bump.run

        Diff args () ->
          Diff.run args

        Repl () flags ->
          Repl.run flags

        Dev () flags ->
          Develop.run flags


attemptToRun :: String -> [String] -> IO ()
attemptToRun command args =
  do  found <- Dir.findExecutable ("elm-" ++ command)
      case found of
        Nothing ->
          IO.hPutStrLn IO.stderr $
            "Could not find command `" ++ command ++ "`. Maybe there is a typo?\n\n\
            \Default commands include:\n\n"
            ++ availableCommands ++
            "\nWhen you try to run the command `" ++ command ++ "` we actually search for an\n\
            \  executable named elm-" ++ command ++ ". Are you able to run elm-" ++ command ++ "?\n\
            \  Is it on your PATH?"

        Just path ->
          do  (_, _, _, handle) <- Proc.createProcess (Proc.proc path args)
              exitWith =<< Proc.waitForProcess handle


usageMessage :: String
usageMessage =
  "Elm Platform " ++ Pkg.versionToString Compiler.version ++ " - a way to run all Elm tools\n\
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
