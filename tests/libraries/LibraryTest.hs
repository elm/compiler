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
  setCurrentDirectory $ top </> "tests" </> "libraries"
  let localCompiler = joinPath [ top, "dist", "build", "elm", "elm" ]
  runCmd localCompiler [ "--make"
                       , "--only-js"
                       , "--src-dir=" ++ top </> "automaton"
                       , "--src-dir=" ++ top </> "IO"
                       , "--src-dir=" ++ top </> "Elm-Test"
                       , "Test.elm"
                       ]

  -- run tests
  let ioScript s = top </> "IO" </> s
      files = [ ioScript "prescript.js"
              , Elm.runtime
              , "build" </> "Test.js"
              , ioScript "handler.js"
              ]
  exe <- readProcess "cat" files ""
  writeFile "exe.js" exe
  exitWith =<< waitForProcess =<< (runCommand "node exe.js")

runCmd :: String -> [String] -> IO ()
runCmd cmd args = do
  putStrLn (cmd ++ " " ++ unwords args)
  (exitCode, stdout, stderr) <- readProcessWithExitCode cmd args ""
  putStr stdout
  putStr stderr
  case exitCode of
    ExitSuccess   -> return ()
    ExitFailure _ -> exitWith exitCode
