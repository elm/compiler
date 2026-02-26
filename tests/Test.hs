module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Parse as Parse
import qualified Test.Compile as Compile


main :: IO ()
main =
  defaultMain $
    testGroup "Elm Tests"
      [ Parse.tests
      , Compile.tests
      ]
