module Test.Dict (tests) where

import Dict
import List

import ElmTest.Assertion (..)
import ElmTest.Test (..)

animals : Dict.Dict String String
animals = Dict.fromList [ ("Tom", "cat"), ("Jerry", "mouse") ]

tests : [Test]
tests =
  let getTests =
        [ test "get 1" <| assertEqual (Just "cat") (Dict.get "Tom" animals)
        , test "get 2" <| assertEqual Nothing (Dict.get "Spike" animals)
        , test "getSafe 1" <| assertEqual "mouse" (Dict.getSafe "dog" "Jerry" animals)
        , test "getSafe 2" <| assertEqual "dog" (Dict.getSafe "dog" "Spike" animals)
        , test "getUnsafe" <| assertEqual "cat" (Dict.getUnsafe "Tom" animals)
        ]
      filterTests =
        [ test "filter" <| assertEqual (Dict.singleton "Tom" "cat") (Dict.filter (\k v -> k == "Tom") animals)
        , test "partition" <| assertEqual (Dict.singleton "Tom" "cat", Dict.singleton "Jerry" "mouse") (Dict.partition (\k v -> k == "Tom") animals)
        ]
  in
      List.concat [ getTests, filterTests ]
