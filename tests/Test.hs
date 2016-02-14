module Main where

import Test.Framework

import Test.Compiler
--import Test.Property

import Parse.ExpressionTest
import Parse.LiteralTest
import Parse.PatternTest
import Parse.TypeTest


main :: IO ()
main =
  defaultMain
    [ compilerTests
--    , propertyTests

    , Parse.ExpressionTest.tests
    , Parse.LiteralTest.tests
    , Parse.PatternTest.tests
    , Parse.TypeTest.tests
    ]
