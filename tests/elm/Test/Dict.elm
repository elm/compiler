module Test.Dict (tests) where

import Dict
import List
import Char

import ElmTest.Assertion (..)
import ElmTest.Test (..)

parseDigit : Char -> Int
parseDigit ch = Char.toCode ch - 48

dict1List = [(1,'1'),(2,'2'),(3,'3'),(5,'5')]

dict1 : Dict Int Char
dict1 = Dict.fromList dict1List

dict1Part1 : Dict Int Char
dict1Part1 = Dict.fromList (take 2 dict1List)

dict1Part2 : Dict Int Char
dict1Part2 = Dict.fromList (drop 2 dict1List)

tests : [Test]
tests =
    let
      filterTests = [assertEqual dict1Part2 <| filter (\v -> parseDigit v > 2) dict1]
      partitionTests = [assertEqual (dict1Part1, dict1Part2) <| partition (\v -> v != '3' && v != '5') dict1]
    in List.concat [filterTests, partitionTests]
