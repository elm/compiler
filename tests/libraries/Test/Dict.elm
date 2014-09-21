module Test.Dict (tests) where

import Dict
import List

import ElmTest.Assertion (..)
import ElmTest.Test (..)

animals : Dict.Dict String String
animals = Dict.fromList [ ("Tom", "cat"), ("Jerry", "mouse") ]

tests : Test
tests =
  let buildTests = suite "build Tests"
        [ test "empty" <| assertEqual (Dict.fromList []) (Dict.empty)
        , test "singleton" <| assertEqual (Dict.fromList [("k","v")]) (Dict.singleton "k" "v")
        , test "insert" <| assertEqual (Dict.fromList [("k","v")]) (Dict.insert "k" "v" Dict.empty)
        , test "insert replace" <| assertEqual (Dict.fromList [("k","vv")]) (Dict.insert "k" "vv" (Dict.singleton "k" "v"))
        , test "update" <| assertEqual (Dict.fromList [("k","vv")]) (Dict.update "k" (\v->Just "vv") (Dict.singleton "k" "v"))
        , test "update Nothing" <| assertEqual Dict.empty (Dict.update "k" (\v->Nothing) (Dict.singleton "k" "v"))
        , test "remove" <| assertEqual Dict.empty (Dict.remove "k" (Dict.singleton "k" "v"))
        , test "remove not found" <| assertEqual (Dict.singleton "k" "v") (Dict.remove "kk" (Dict.singleton "k" "v"))
        ]
      queryTests = suite "query Tests"
        [ test "member 1" <| assertEqual True (Dict.member "Tom" animals)
        , test "member 2" <| assertEqual False (Dict.member "Spike" animals)
        , test "get 1" <| assertEqual (Just "cat") (Dict.get "Tom" animals)
        , test "get 2" <| assertEqual Nothing (Dict.get "Spike" animals)
        , test "getOrElse 1" <| assertEqual "mouse" (Dict.getOrElse "dog" "Jerry" animals)
        , test "getOrElse 2" <| assertEqual "dog" (Dict.getOrElse "dog" "Spike" animals)
        , test "getOrFail" <| assertEqual "cat" (Dict.getOrFail "Tom" animals)
        ]
      combineTests = suite "combine Tests"
        [ test "union" <| assertEqual animals (Dict.union (Dict.singleton "Jerry" "mouse") (Dict.singleton "Tom" "cat"))
        , test "union collison" <| assertEqual (Dict.singleton "Tom" "cat") (Dict.union (Dict.singleton "Tom" "cat") (Dict.singleton "Tom" "mouse"))
        , test "intersect" <| assertEqual (Dict.singleton "Tom" "cat") (Dict.intersect animals (Dict.singleton "Tom" "cat"))
        , test "diff" <| assertEqual (Dict.singleton "Jerry" "mouse") (Dict.diff animals (Dict.singleton "Tom" "cat"))
        ]
      transformTests = suite "transform Tests"
        [ test "filter" <| assertEqual (Dict.singleton "Tom" "cat") (Dict.filter (\k v -> k == "Tom") animals)
        , test "partition" <| assertEqual (Dict.singleton "Tom" "cat", Dict.singleton "Jerry" "mouse") (Dict.partition (\k v -> k == "Tom") animals)
        ]
  in
    suite "Dict Tests"
    [ buildTests
    , queryTests
    , combineTests
    , transformTests
    ]
