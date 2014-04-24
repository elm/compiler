module Test.List (tests) where

import List

import ElmTest.Assertion (..)
import ElmTest.Test (..)

tests : [Test]
tests = 
  let partitionTests = 
          let p1 = id
              p2 x = x < 3
              isEven n = n `mod` 2 == 0
          in [ test "simple partition" <| assertEqual ([True],[False]) (List.partition p1 [False, True]),
               test "order check"      <| assertEqual ([2,1], [5,6])   (List.partition p2 [2,5,6,1]),
               test "partition doc check 1"      <| assertEqual ([0,1,2], [3,4,5]) (partition p2 [0..5]),
               test "partition doc check 2"      <| assertEqual ([0,2,4], [1,3,5]) (partition isEven [0..5])
             ]
      unzipTests = [
       test "unzip doc check" <| assertEqual ([0,17,1337],[True,False,True]) (unzip [(0, True), (17, False), (1337, True)])
      ]
      intersperseTests = [
       test "intersperse doc check" <| assertEqual ["turtles","on","turtles","on","turtles"] (intersperse "on" ["turtles","turtles","turtles"])
      ]
  in List.concat [partitionTests, unzipTests, intersperseTests]