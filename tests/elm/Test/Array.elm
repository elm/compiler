module Test.Array (tests) where

import List
import Array

import ElmTest.Assertion (..)
import ElmTest.Test (..)

tests : [Test]
tests =
    let creationTests = [ test "empty" <| assertEqual Array.empty (Array.fromList [])
                        , test "initialize" <| assertEqual (Array.initialize 4 id) (Array.fromList [0,1,2,3])
                        , test "initialize 2" <| assertEqual (Array.initialize 4 (\n -> n*n)) (Array.fromList [0,1,4,9])
                        , test "initialize 3" <| assertEqual (Array.initialize 4 (always 0)) (Array.fromList [0,0,0,0])
                        , test "initialize Empty" <| assertEqual (Array.initialize 0 id) Array.empty
                        , test "initialize 4" <| assertEqual (Array.initialize 2 (always 0)) (Array.fromList [0,0])
                        , test "repeat" <| assertEqual (Array.repeat 5 40) (Array.fromList [40,40,40,40,40])
                        , test "repeat 2" <| assertEqual (Array.repeat 5 0) (Array.fromList [0,0,0,0,0])
                        , test "repeat 3" <| assertEqual (Array.repeat 3 "cat") (Array.fromList ["cat","cat","cat"])
                        , test "fromList" <| assertEqual (Array.fromList []) Array.empty
                        ]
        basicsTests = [ test "length" <| assertEqual (Array.length (Array.fromList [1,2,3])) 3
                      , test "length - Long" <| assertEqual (Array.length (Array.repeat 10000 0)) 10000
                      , test "push" <| assertEqual (Array.push 3 (Array.fromList [1,2])) (Array.fromList [1,2,3])
                      , test "append" <| assertEqual (Array.toList <| Array.append (Array.repeat 2 42) (Array.repeat 3 81)) ([42,42,81,81,81])
                      ]
        getAndSetTests = [ test "get" <| assertEqual (Array.get 2 (Array.fromList [3,2,1])) 1
                         , test "getMaybe" <| assertEqual (Array.getMaybe 1 (Array.fromList [3,2,1])) (Just 2)
                         , test "getMaybe 2" <| assertEqual (Array.getMaybe 5 (Array.fromList [3,2,1])) Nothing
                         , test "getMaybe 3" <| assertEqual (Array.getMaybe -1 (Array.fromList [3,2,1])) Nothing
                         , test "getWithDefault" <| assertEqual (Array.getWithDefault 0 2 (Array.fromList [3,2,1])) 1
                         , test "getWithDefault 2" <| assertEqual (Array.getWithDefault 0 5 (Array.fromList [3,2,1])) 0
                         , test "set" <| assertEqual (Array.set 1 7 (Array.fromList [1,2,3])) (Array.fromList [1,7,3])
                         ]
        takingArraysApartTests = [ test "toList" <| assertEqual (Array.toList (Array.fromList [3,5,8])) [3,5,8]
                                 , test "toIndexedList" <| assertEqual (Array.toIndexedList (Array.fromList ["cat","dog"])) [(0,"cat"), (1,"dog")]
                                 , test "slice" <| assertEqual (Array.slice 1 2 (Array.fromList [0,1,2,3,4])) (Array.fromList [1,2])
                                 , test "slice 2" <| assertEqual (Array.slice 1 -2 (Array.fromList [0,1,2,3,4])) (Array.fromList [1,2,3])
                                 , test "slice 3" <| assertEqual (Array.slice -3 -2 (Array.fromList [0,1,2,3,4])) (Array.fromList [2,3])
                                 ]
        mappingAndFoldingTests = [ test "map" <| assertEqual (Array.map sqrt (Array.fromList [1,4,9])) (Array.fromList [1,2,3])
                                 , test "indexedMap" <| assertEqual (Array.indexedMap (*) (Array.fromList [5,5,5])) (Array.fromList [0,5,10])
                                 , test "foldl" <| assertEqual (Array.foldl (::) [] (Array.fromList [1,2,3])) [3,2,1]
                                 , test "foldr" <| assertEqual (Array.foldr (+) 0 (Array.repeat 3 5)) 15
                                 , test "foldr 2" <| assertEqual (Array.foldr (::) [] (Array.fromList [1,2,3])) [1,2,3]
                                 , test "filter" <| assertEqual (Array.filter (\x -> x `mod` 2 == 0) (Array.fromList [1..6])) (Array.fromList [2,4,6])
                                 ]
    in List.concat [creationTests, basicsTests, getAndSetTests, takingArraysApartTests, mappingAndFoldingTests]
