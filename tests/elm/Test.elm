module Main where

import ElmTest.Assertion as A
import ElmTest.Run as R
import ElmTest.Runner.Console (runDisplay)
import ElmTest.Test (..)

import IO.IO (..)
import IO.Runner (Request, Response)
import IO.Runner as Run

import Test.Array as Array
import Test.Dict as Dict
import Test.Eq as Eq
import Test.List as List
import Test.Set as Set
import Test.String as String
import Test.Trampoline as Trampoline

tests : Test
tests = suite "Elm Standard Library Tests" [ List.tests,
                                             Eq.tests,
                                             String.tests, 
                                             Trampoline.tests,
                                             Array.tests,
                                             Dict.tests,
                                             Set.tests
                                           ]

console : IO ()
console = runDisplay tests

port requests : Signal [{ mPut  : Maybe String
                        , mExit : Maybe Int
                        , mGet  : Bool
                        }]
port requests = Run.run responses console

port responses : Signal (Maybe String)
