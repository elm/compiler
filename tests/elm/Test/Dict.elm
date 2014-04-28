module Test.Dict (tests) where

import Dict
import List

import ElmTest.Assertion (..)
import ElmTest.Test (..)

animals : Dict.Dict String String
animals = Dict.fromList [ ("Tom", "cat"), ("Jerry", "mouse") ]

tests : Test
tests =
  let getTests =
        [ test "get 1" <| assertEqual (Just "cat") (Dict.get "Tom" animals)
        , test "get 2" <| assertEqual Nothing (Dict.get "Spike" animals)
        , test "getOrElse 1" <| assertEqual "mouse" (Dict.getOrElse "dog" "Jerry" animals)
        , test "getOrElse 2" <| assertEqual "dog" (Dict.getOrElse "dog" "Spike" animals)
        , test "getOrFail" <| assertEqual "cat" (Dict.getOrFail "Tom" animals)
        ]
      filterTests =
        [ test "filter" <| assertEqual (Dict.singleton "Tom" "cat") (Dict.filter (\k v -> k == "Tom") animals)
        , test "partition" <| assertEqual (Dict.singleton "Tom" "cat", Dict.singleton "Jerry" "mouse") (Dict.partition (\k v -> k == "Tom") animals)
        ]
  in
    suite "Dict Tests" <| List.concat [ getTests, filterTests ]
