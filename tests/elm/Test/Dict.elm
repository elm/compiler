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
        [ test "get" <| assertEqual "cat" (Dict.get "Tom" animals)
        , test "getMaybe 1" <| assertEqual (Just "cat") (Dict.getMaybe "Tom" animals)
        , test "getMaybe 2" <| assertEqual Nothing (Dict.getMaybe "Spike" animals)
        , test "getWithDefault 1" <| assertEqual "mouse" (Dict.getWithDefault "dog" "Jerry" animals)
        , test "getWithDefault 2" <| assertEqual "dog" (Dict.getWithDefault "dog" "Spike" animals)
        ]
      filterTests =
        [ test "filter" <| assertEqual (Dict.singleton "Tom" "cat") (Dict.filter (\k v -> k == "Tom") animals)
        , test "partition" <| assertEqual (Dict.singleton "Tom" "cat", Dict.singleton "Jerry" "mouse") (Dict.partition (\k v -> k == "Tom") animals)
        ]
  in
      List.concat [ getTests, filterTests ]
