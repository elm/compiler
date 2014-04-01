module Main where

import ElmTest.Assertion as A
import ElmTest.Run as R
import ElmTest.Runner.Console (runDisplay)
import ElmTest.Test (..)

import IO.IO (..)
import IO.Runner (Request, Response)
import IO.Runner as Run

{- Failing test to get it going -}
tests : [Test]
tests = [ 0 `equals` 1 ]     

console : IO ()
console = runDisplay tests

port requests : Signal [{ mPut  : Maybe String
                        , mExit : Maybe Int
                        , mGet  : Bool
                        }]
port requests = Run.run responses console

port responses : Signal (Maybe String)
