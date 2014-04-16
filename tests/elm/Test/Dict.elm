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

dict2List : Dict Int Char
dict2List = [(1,'2'),(3,'3'),(7,'4'),(8,'3')]

dict2 : Dict Int Char
dict2 = Dict.fromList dict2List

dict2Part1 : Dict Int Char
dict2Part1 = Dict.fromList (take 2 dict2List)

dict2Part2 : Dict Int Char
dict2Part2 = Dict.fromList (drop 2 dict2List)

tests : [Test]
tests =
    let
      partitionTests = [assertEqual (dict1Part1, dict1Part2) <| partition (\v -> v != "3" && v != "5") dict1]
      partitionWithKeyTests = [assertEqual (dict2Part1, dict2Part2) <| partition (\k v -> k <= parseDigit v)]
      filterTests = [assertEqual dict1Part2 <| filter (\v -> parseDigit v > 2) dict1]
      filterWithKeyTests = [assertEqual dict2Part2 <| filterWithKey (\k _ -> k > 6) dict2]
    in List.concat [partitionTests, partitionWithKeyTests, filterTests, filterWithKeyTests]
