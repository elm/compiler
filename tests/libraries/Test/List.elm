module Test.List (tests) where

import List
import String

import ElmTest.Assertion (..)
import ElmTest.Test (..)

largeNumber = 100000
trueList = repeat largeNumber True
falseList = repeat largeNumber False

lessThanThree x = x < 3
isEven n = n % 2 == 0

alice = { name="Alice", height=1.62 }
bob   = { name="Bob"  , height=1.85 }
chuck = { name="Chuck", height=1.76 }

flippedComparison a b =
  case compare a b of
    LT -> GT
    EQ -> EQ
    GT -> LT

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
      concatTests = suite "concat Tests"
        [ test "concat doc check 1" <| assertEqual [1,2,3,4,5] (concat [[1,2],[3],[4,5]])
        , test "concat doc check 2" <| assertEqual "treehouse" (concat ["tree","house"])
        ]
      joinTests = suite "join Tests"
        [ test "join doc check" <| assertEqual "Hawaiian" (join "a" ["H","w","ii","n"])
        , test "join list check" <| assertEqual [1,0,2,0,3] (join [0] [[1],[2],[3]])
        ]
      intersperseTests = suite "intersperse Tests"
        [ test "intersperse doc check" <| assertEqual ["turtles","on","turtles","on","turtles"] (intersperse "on" ["turtles","turtles","turtles"])
        , test "intersperse stress test" <| assertEqual (tail <| List.concat <| zipWith (\x y -> [x,y]) falseList trueList) (intersperse False trueList)
        ]
      zipTests = suite "zip Tests"
        [ test "zip doc check 1" <| assertEqual [(1,6),(2,7)] (zip [1,2,3] [6,7])
        , test "zip doc check 2" <| assertEqual (zipWith (,) [1,2,3] [6,7]) (zip [1,2,3] [6,7])
        ]
      filterMapTests = suite "filterMap Tests"
        [ test "filterMap doc check" <| assertEqual [3,5] (filterMap String.toInt ["3","4.0","5","hats"])
        ]
      concatMapTests = suite "concatMap Tests"
        [ test "simple concatMap check" <| assertEqual [1,1,2,2] (concatMap (repeat 2) [1,2])
        ]
      indexedMapTests = suite "indexedMap Tests"
        [ test "indexedMap doc check" <| assertEqual [(0,"Tom"),(1,"Sue"),(2,"Bob")] (indexedMap (,) ["Tom", "Sue", "Bob"])
        ]
      sortTests = suite "sort Tests"
        [ test "sort doc check" <| assertEqual [1,3,5] (sort [3,1,5])
        , test "sort string check" <| assertEqual ["a","c","e"] (sort ["c","a","e"])
        ]
      sortByTests = suite "sortBy Tests"
        [ test "sortBy doc check" <| assertEqual ["cat","mouse"] (sortBy String.length ["mouse","cat"])
        , test "sortby derived property check 1" <| assertEqual [alice,bob,chuck] (sortBy .name [chuck,alice,bob])
        , test "sortby derived property check 2" <| assertEqual [alice,chuck,bob] (sortBy .height [chuck,alice,bob])
        ]
      sortWithTests = suite "sortWith Tests"
        [ test "sortWith doc check" <| assertEqual [5,4,3,2,1] (sortWith flippedComparison [1..5])
        ]
  in
      suite "List Tests"
      [ partitionTests
      , unzipTests
      , concatTests
      , joinTests
      , intersperseTests
      , zipTests
      , filterMapTests
      , concatMapTests
      , indexedMapTests
      , sortTests
      , sortByTests
      , sortWithTests
      ]
