module Main where

import ElmTest.Assertion as A
import ElmTest.Run as R
import ElmTest.Runner.Console (runDisplay)
import ElmTest.Test (..)

import IO.IO (..)
import IO.Runner (Request, Response)
import IO.Runner as Run

import Test.String as String
import Test.Trampoline as Trampoline
import Test.Array as Array
    
tests : [Test]
tests = String.tests ++ Trampoline.tests ++ Array.tests

console : IO ()
console = runDisplay tests

port requests : Signal [{ mPut  : Maybe String
                        , mExit : Maybe Int
                        , mGet  : Bool
                        }]
port requests = Run.run responses console

port responses : Signal (Maybe String)
