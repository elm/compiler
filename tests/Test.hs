module Main where

import Test.Framework

import Test.Compiler
--import Test.Property

import Parse.LiteralTest


main :: IO ()
main =
  defaultMain
    [ compilerTests
--    , propertyTests

    , Parse.LiteralTest.tests
    ]
