module Test.List (tests) where

import List

import ElmTest.Assertion (..)
import ElmTest.Test (..)

largeNumber = 100000
trueList = repeat largeNumber True
falseList = repeat largeNumber False

lessThanThree x = x < 3
isEven n = n % 2 == 0

tests : Test
tests = 
  let partitionTests = suite "partition Tests"
        [ test "simple partition" <| assertEqual ([True],[False]) (List.partition identity [False, True])
        , test "order check" <| assertEqual ([2,1], [5,6]) (List.partition lessThanThree [2,5,6,1])
        , test "partition doc check 1" <| assertEqual ([0,1,2], [3,4,5]) (partition lessThanThree [0..5])
        , test "partition doc check 2" <| assertEqual ([0,2,4], [1,3,5]) (partition isEven [0..5])
        , test "partition stress test" <| assertEqual (trueList, falseList) (partition identity (falseList ++ trueList))
        ]
      unzipTests = suite "unzip Tests"
        [ test "unzip doc check" <| assertEqual ([0,17,1337],[True,False,True]) (unzip [(0, True), (17, False), (1337, True)])
        , test "unzip stress test" <| assertEqual (trueList, falseList) (unzip (zip trueList falseList))
        ]
      intersperseTests = suite "intersperse Tests"
        [ test "intersperse doc check" <| assertEqual ["turtles","on","turtles","on","turtles"] (intersperse "on" ["turtles","turtles","turtles"])
        , test "intersperse stress test" <| assertEqual (tail . List.concat <| zipWith (\x y -> [x,y]) falseList trueList) (intersperse False trueList)
        ]
  in
      suite "List Tests"
      [ partitionTests
      , unzipTests
      , intersperseTests
      ]
