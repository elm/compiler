module Main where

import Control.Monad (unless)
import Data.List (isInfixOf)
import System.Directory
import System.Exit
import System.FilePath
import System.Process

import Elm.Internal.Paths as Elm

main :: IO ()
main = do
  top <- getCurrentDirectory

  -- setup submodules and npm
  runCmd "git" ["submodule", "update", "--init"]
  out <- readProcess "npm" ["ls", "--parseable"] ""
  unless ("jsdom" `isInfixOf` out) $ runCmd "npm" ["install","jsdom"]

  -- setup actual library tests
  let localCompiler = joinPath [ "dist", "build", "elm", "elm" ]
  runCmd localCompiler [ "--make"
                       , "--only-js"
                       , "--src-dir=IO"
                       , "--src-dir=Elm-Test"
                       , "--src-dir=" ++ "tests" </> "libraries"
                       , testFile <.> "elm"
                       ]

  -- run tests
  let files = [ "IO" </> "share" </> "prescript.js"
              , Elm.runtime
              , "build" </> testFile <.> "js"
              , "IO" </> "share" </> "handler.js"
              ]
      testRunner = "build" </> "TestRunner.js"
  exe <- readProcess "cat" files ""
  writeFile testRunner exe
  exitCode <- rawSystem "node" [testRunner]
  removeDirectoryRecursive "cache"
  removeDirectoryRecursive "build"
  exitWith exitCode
  where
    testFile = "tests" </> "libraries" </> "Test"

runCmd :: String -> [String] -> IO ()
runCmd cmd args = do
  putStrLn (cmd ++ " " ++ unwords args)
  (exitCode, stdout, stderr) <- readProcessWithExitCode cmd args ""
  putStr stdout
  putStr stderr
  case exitCode of
    ExitSuccess   -> return ()
    ExitFailure _ -> exitWith exitCode
