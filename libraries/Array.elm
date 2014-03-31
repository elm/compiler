module Array where

{-| A library for manipulating arrays of elements. The elements in an
array must have the same type. The arrays are implemented in Relaxed
Radix Balanced-Trees for fast updating and concating.

If you use more then one map or zip function on an array, consider turning it
into a list before operating on it, and then turning it back into an array.

# Basics
@docs empty, length, get, safeGet, getWithDefault

# Putting Arrays Together
@docs fill, fromList, concat, set, updates, push

# Taking Arrays Apart
@docs elements, indices, assocs, slice

# Mapping, folding, zipping
@docs map, assocMap, foldl, foldr, zip, zipWith
-}

import Native.Array
import Basics (..)
import Maybe (..)
import List

data Array a = Array

{-| Fills an array with a given length and a default element.

      fill 3 5 == fromList [5,5,5]
-}
fill : Int -> a -> Array a
fill len e = fromList <| List.map (always e) [1..len]

-- TODO: make this a native function.
{-| Create an array from a list. -}
fromList : [a] -> Array a
fromList = List.foldl (Native.Array.push) Native.Array.empty

{-| Create a list of elements from an array.

      elements (fromList [3,5,8]) == [3,5,8]
-}
elements : Array a -> [a]
elements = Native.Array.toList

{-| Create a list of the possible indices of an array.

      indices (fromList [3,5,8] == [0,1,2])
-}
indices : Array a -> [Int]
indices a = [0..Native.Array.length a - 1]

{-| Create a list of tuples (Index, Element) from an array.

      assocs (fromList [3,5,8]) == [(0,3), (1,5), (2,8)]
-}
assocs : Array a -> [(Int, a)]
assocs a = List.zip (indices a) (Native.Array.elements a)

{-| Apply a function on every element in an array.

      map (\x -> x*x) (fromList [1,2,3]) == fromList [1,4,9]
-}
map : (a -> b) -> Array a -> Array b
map = Native.Array.map

{-| Apply a function with an index on every element in an array.

      assocMap (*) (fromList [5,5,5]) == fromList [0,5,10]
-}
assocMap : (Int -> a -> b) -> Array a -> Array b
assocMap = Native.Array.assocMap

{-| Reduce an array from the left.

      foldl (+) 0 (fill 3 5) == 15
-}
foldl : (a -> b -> b) -> b -> Array a -> b
foldl = Native.Array.foldl

{-| Reduce an array from the right.

      foldr (::) [] (fromList [1,2,3]) == [3,2,1]
-}
foldr : (a -> b -> b) -> b -> Array a -> b
foldr = Native.Array.foldr

{-| Zip the elements of two arrays via a tuple into a new array. If one array is
longer, the extra elements are dropped.

      zip (fromList [1,2,3,4]) (fromList [0,0,0]) = fromList [(1,0), (2,0), (3,0)]
-}
zip : Array a -> Array b -> Array (a,b)
zip = zipWith (,)

{-| Zips the elements of two arrays via a function into a new array. If one array
is longer, the extra elements are dropped.

      zipWith (+) (fill 3 5) (fill 5 8) == fill 3 13
-}
zipWith : (a -> b -> c) -> Array a -> Array b -> Array c
zipWith f a b =
  fromList <| List.zipWith f (Native.Array.elements a) (Native.Array.elements b)

{-| Return an empty array.

      length empty == 0
-}
empty : Array a
empty = Native.Array.empty

{-| Push an element to the end of an array.

      push 3 (fromList [1,2]) == fromList [1,2,3]
-}
push : a -> Array a -> Array a
push = Native.Array.push

{-| Return the element at the index. Breaks, if index is out of range. If the
array length is unkown, use safeGet oder getWithDefault.

      get 2 (A.fromList [3,2,1]) == 1
-}
get : Int -> Array a -> a
get = Native.Array.get

{-| Return Just the element at the index or Nothing if the index is out of range.

      getMaybe 2 (A.fromList [3,2,1]) == Just 2
      getMaybe 5 (A.fromList [3,2,1]) == Nothing
-}
getMaybe : Int -> Array a -> Maybe a
getMaybe i array = if Native.Array.length array > i
                  then Just (Native.Array.get i array)
                  else Nothing

{-| Get the element at the index. If the index is out of range, the given default
element is returned.

      getSafe 0 2 (A.fromList [3,2,1]) == 1
      getSafe 0 5 (A.fromList [3,2,1]) == 0
-}
getSafe : a -> Int -> Array a -> a
getSafe default i array = if Native.Array.length array > i
                                 then Native.Array.get i array else default

{-| Set the element at the index. Returns the updated array, or if the index is
out of range, the unaltered array.

      set 1 7 (fromList [1,2,3]) == fromList [1,7,3]
-}
set : Int -> a -> Array a -> Array a
set = Native.Array.set

{-| Update an array with a a list tuples, wherein the first Int is the index.

      updates [(1,7),(2,8)] (fill 3 1) == fromList [1,7,8]
-}
updates : [(Int, a)] -> Array a -> Array a
updates assocs array = List.foldl (uncurry Native.Array.set) array assocs

{-| Slice an array given a range. The selection is inclusive, so the last
element in the selection will also be in the new array. This may change in the
future.
You can select from the end by giving a negative Int.

      slice 1 2   (fromList [0,1,2,3,4]) == fromList [1,2]
      slice 1 -2  (fromList [0,1,2,3,4]) == fromList [1,2,3]
      slice -3 -2 (fromList [0,1,2,3,4]) == fromList [2,3]
-}
slice : Int -> Int -> Array a -> Array a
slice = Native.Array.slice

{-| Return the length of an array.

      length (fromList [1,2,3]) == 3
-}
length : Array a -> Int
length = Native.Array.length

{-| Concat two arrays to a new one.

      concat (fill 3 1) (fill 4 2) == fromList [1,1,1,2,2,2,2]
-}
concat : Array a -> Array a -> Array a
concat = Native.Array.concat
