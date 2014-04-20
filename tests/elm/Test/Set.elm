module Test.Set (tests) where

import Set
import List

import ElmTest.Assertion (..)
import ElmTest.Test (..)

set : Set
set = Set.fromList [1..100]

setPart1 : Set
setPart1 = Set.fromList [1..50]

setPart2 : Set
setPart2 = Set.fromList [51..100]

pred : Int -> Bool
pred x = x <= 50

tests : [Test]
tests =
    let
      filterTests = [assertEqual setPart1 <| filter pred set]
      partitionTests = [assertEqual (setPart1, setPart2) <| partition pred set]
    in List.concat [partitionTests, filterTests]
