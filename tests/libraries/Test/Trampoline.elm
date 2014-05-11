module Test.Trampoline (tests) where

import Trampoline (..)

import ElmTest.Assertion (..)
import ElmTest.Test (..)
    
badSum : Int -> Int
badSum n =
    let loop x acc =
            case x of
              0 -> acc
              _ -> loop (x-1) (acc+x)
    in loop n 0

goodSum : Int -> Int
goodSum n =
    let sumT x acc =
            case x of
              0 -> Done acc
              _ -> Continue (\_ -> sumT (x-1) (acc+x))
    in trampoline <| sumT n 0

tests : Test
tests =
    let mkLoopCheck n = test ("Equivalent Loop Check #"++ show n) (assertEqual (badSum n) (goodSum n))
        loopChecks = suite "Equivalent to untrampolined Tests" <| map mkLoopCheck [0..25]
        noStackOverflow = test "Trampoline Deep Recursion Test" (assertEqual 500000500000 (goodSum 1000000))
    in suite "Trampoline Tests" [noStackOverflow, loopChecks]
