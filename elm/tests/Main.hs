module Main where

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Maybe (listToMaybe)
import Data.Functor.Identity

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [
    testGroup "The" [
      testProperty "prop_blah" prop_blah
    ]
  ]

prop_blah :: Bool -> Bool
prop_blah _ = True