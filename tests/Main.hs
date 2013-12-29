module Main where

import Test.Framework

import Tests.Compiler
import Tests.Property

main :: IO () 
main = defaultMain [ compilerTests
                   , propertyTests
                   ]
