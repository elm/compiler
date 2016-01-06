module Main where

import Test.Framework

import Test.Compiler
--import Test.Property

import Parse.LiteralTest
import Parse.TypeTest


main :: IO ()
main =
  defaultMain
    [ compilerTests
--    , propertyTests

    , Parse.LiteralTest.tests
    , Parse.TypeTest.tests
    ]
