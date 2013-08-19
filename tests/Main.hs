module Main where

import System.Directory
import System.Exit (exitWith)
import System.Environment (getArgs)
import Test.Framework.TestManager
import Test.Framework.BlackBoxTest

main :: IO () 
main = do
  args  <- getArgs
  tests <- blackBoxTests "tests/good" "dist/build/elm/elm" ".elm" bbtArgs
  code  <- runTestWithArgs args tests
  removeDirectoryRecursive "cache"
  removeDirectoryRecursive "build"
  exitWith code

bbtArgs = defaultBBTArgs { bbtArgs_stdoutDiff = ignoreDiff
                         , bbtArgs_stderrDiff = ignoreDiff }

ignoreDiff :: Diff
ignoreDiff _ _ = return Nothing