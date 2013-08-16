module Main where

import Test.Framework
import Test.Framework.BlackBoxTest

main :: IO () 
main = do
  tests <- blackBoxTests "tests/good" "dist/build/elm/elm" ".elm" args
  htfMain tests

args = defaultBBTArgs { bbtArgs_stdoutDiff = ignoreDiff
                      , bbtArgs_stderrDiff = ignoreDiff }

ignoreDiff :: Diff
ignoreDiff _ _ = return Nothing