{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import System.Directory
import System.Exit (exitWith)
import System.Environment (getArgs)

import Test.Framework
import Test.Framework.TestManager
import Test.Framework.BlackBoxTest

import {-@ HTF_TESTS @-} qualified TestInterfaceSerialization

main :: IO ()
main = do
  args  <- getArgs
  bbts  <- blackBoxTests "tests" "dist/build/elm/elm" ".elm" bbtArgs
  code  <- runTestWithArgs args $ htf_importedTests ++ [makeAnonTestSuite bbts]

  removeDirectoryRecursive "cache"
  removeDirectoryRecursive "build"

  exitWith code

bbtArgs = defaultBBTArgs { bbtArgs_stdoutDiff = ignoreDiff
                         , bbtArgs_stderrDiff = ignoreDiff }

ignoreDiff :: Diff
ignoreDiff _ _ = return Nothing
