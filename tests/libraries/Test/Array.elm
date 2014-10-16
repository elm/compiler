module Test.Array (tests) where

import List
import Array
import Native.Array

import ElmTest.Assertion (..)
import ElmTest.Test (..)

mergeSplit : Int -> Array.Array a -> Array.Array a
mergeSplit n arr =
  let left = Array.slice 0 n arr
      right = Array.slice n (Array.length arr) arr
  in Array.append left right

holeArray : Array.Array Int
holeArray = foldl mergeSplit (Array.fromList [0..100]) [0..100]

tests : Test
tests =
  let creationTests = suite "Creation"
        [ test "empty" <| assertEqual Array.empty (Array.fromList [])
        , test "initialize" <| assertEqual (Array.initialize 4 identity) (Array.fromList [0,1,2,3])
        , test "initialize 2" <| assertEqual (Array.initialize 4 (\n -> n*n)) (Array.fromList [0,1,4,9])
        , test "initialize 3" <| assertEqual (Array.initialize 4 (always 0)) (Array.fromList [0,0,0,0])
        , test "initialize Empty" <| assertEqual (Array.initialize 0 identity) Array.empty
        , test "initialize 4" <| assertEqual (Array.initialize 2 (always 0)) (Array.fromList [0,0])
        , test "repeat" <| assertEqual (Array.repeat 5 40) (Array.fromList [40,40,40,40,40])
        , test "repeat 2" <| assertEqual (Array.repeat 5 0) (Array.fromList [0,0,0,0,0])
        , test "repeat 3" <| assertEqual (Array.repeat 3 "cat") (Array.fromList ["cat","cat","cat"])
        , test "fromList" <| assertEqual (Array.fromList []) Array.empty
        ]
      basicsTests = suite "Basics"
        [ test "length" <| assertEqual 3 (Array.length (Array.fromList [1,2,3]))
        , test "length - Long" <| assertEqual 10000 (Array.length (Array.repeat 10000 0))
        , test "push" <| assertEqual (Array.fromList [1,2,3]) (Array.push 3 (Array.fromList [1,2]))
        , test "append" <| assertEqual [42,42,81,81,81] (Array.toList (Array.append (Array.repeat 2 42) (Array.repeat 3 81)))
        , test "appendEmpty 1" <| assertEqual [1..33] (Array.toList (Array.append Array.empty (Array.fromList [1..33])))
        , test "appendEmpty 2" <| assertEqual [1..33] (Array.toList (Array.append (Array.fromList [1..33]) Array.empty))
        , test "appendSmall 1" <| assertEqual [1..33] (Array.toList (Array.append (Array.fromList [1..30]) (Array.fromList [31..33])))
        , test "appendSmall 2" <| assertEqual [1..33] (Array.toList (Array.append (Array.fromList [1..3]) (Array.fromList [4..33])))
        , test "appendAndSlice" <| assertEqual [0..100] (Array.toList holeArray)
        ]
      getAndSetTests = suite "Get and Set"
        [ test "get" <| assertEqual (Just 2) (Array.get 1 (Array.fromList [3,2,1]))
        , test "get 2" <| assertEqual Nothing (Array.get 5 (Array.fromList [3,2,1]))
        , test "get 3" <| assertEqual Nothing (Array.get -1 (Array.fromList [3,2,1]))
        , test "getOrElse 1" <| assertEqual 1 (Array.getOrElse 0 2 (Array.fromList [3,2,1]))
        , test "getOrElse 2" <| assertEqual 0 (Array.getOrElse 0 5 (Array.fromList [3,2,1]))
        , test "getOrFail" <| assertEqual 1 (Array.getOrFail 2 (Array.fromList [3,2,1]))
        , test "set" <| assertEqual (Array.fromList [1,7,3]) (Array.set 1 7 (Array.fromList [1,2,3]))
        ]
      takingArraysApartTests = suite "Taking Arrays Apart"
        [ test "toList" <| assertEqual [3,5,8] (Array.toList (Array.fromList [3,5,8]))
        , test "toIndexedList" <| assertEqual [(0,"cat"), (1,"dog")] (Array.toIndexedList (Array.fromList ["cat","dog"]))
        , test "slice 1" <| assertEqual (Array.fromList [0,1,2]) (Array.slice  0  3 (Array.fromList [0,1,2,3,4]))
        , test "slice 2" <| assertEqual (Array.fromList [1,2,3]) (Array.slice  1  4 (Array.fromList [0,1,2,3,4]))
        , test "slice 3" <| assertEqual (Array.fromList [1,2,3]) (Array.slice  1 -1 (Array.fromList [0,1,2,3,4]))
        , test "slice 4" <| assertEqual (Array.fromList [2])     (Array.slice -3 -2 (Array.fromList [0,1,2,3,4]))
        , test "slice 5" <| assertEqual 63 (Array.length <| Array.slice 65 (65 + 63) <| Array.fromList [1..200])
        ]
      mappingAndFoldingTests = suite "Mapping and Folding"
        [ test "map" <| assertEqual (Array.fromList [1,2,3]) (Array.map sqrt (Array.fromList [1,4,9]))
        , test "indexedMap 1" <| assertEqual (Array.fromList [0,5,10]) (Array.indexedMap (*) (Array.fromList [5,5,5]))
        , test "indexedMap 2" <| assertEqual [0..99] (Array.toList (Array.indexedMap always (Array.repeat 100 0)))
        , test "foldl 1" <| assertEqual [3,2,1] (Array.foldl (::) [] (Array.fromList [1,2,3]))
        , test "foldl 2" <| assertEqual 33 (Array.foldl (+) 0 (Array.repeat 33 1))
        , test "foldr 1" <| assertEqual 15 (Array.foldr (+) 0 (Array.repeat 3 5))
        , test "foldr 2" <| assertEqual [1,2,3] (Array.foldr (::) [] (Array.fromList [1,2,3]))
        , test "foldr 3" <| assertEqual 53 (Array.foldr (-) 54 (Array.fromList [10,11]))
        , test "filter" <| assertEqual (Array.fromList [2,4,6]) (Array.filter (\x -> x % 2 == 0) (Array.fromList [1..6]))
        ]
      nativeTests = suite "Conversion to JS Arrays"
        [ test "jsArrays" <| assertEqual (Array.fromList [1..1100]) (Native.Array.fromJSArray (Native.Array.toJSArray (Array.fromList [1..1100])))
        ]
  in
      suite "Array"
        [ creationTests, basicsTests, getAndSetTests
        , takingArraysApartTests, mappingAndFoldingTests, nativeTests
        ]
