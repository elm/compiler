module Main where

import Test.Framework
import Test.Framework.BlackBoxTest

main :: IO () 
main = htfMain tests

tests = blackBoxTests "tests/good" "dist/build/elm/elm" ".elm" defaultBBTArgs