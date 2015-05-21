module Main where

import Test.Framework

import Test.Compiler
--import Test.Property


main :: IO ()
main =
  defaultMain
    [ compilerTests
--    , propertyTests
    ]
