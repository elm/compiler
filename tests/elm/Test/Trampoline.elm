module Test.Trampoline (tests) where

import ElmTest.Test (..)
    
import Trampoline (..)

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

tests = goodSum 1000000 `equals` 500000500000 :: map (\n -> badSum n `equals` goodSum n) [0..50]
        