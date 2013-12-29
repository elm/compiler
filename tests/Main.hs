module Main where

import Test.Framework

import Tests.Compiler

main :: IO () 
main = defaultMain [ compilerTests ]
