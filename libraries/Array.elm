module Array where

{-| A library for fast immutable arrays. The elements in an array must have the
same type. The arrays are implemented in Relaxed Radix Balanced-Trees for fast
reads, updates, and appends.

# Creating Arrays
@docs empty, repeat, initialize, fromList

# Basics
@docs length, push, append

# Get and Set
@docs get, getMaybe, getWithDefault, set

# Taking Arrays Apart
@docs toList, toIndexedList, slice

# Mapping and Folding
@docs map, indexedMap, foldl, foldr
-}

import Native.Array
import Basics (..)
import Maybe (..)
import List

data Array a = Array

{-| Initialize an array. `initialize n f` creates an array of length `n` with
the element at index `i` initialized to the result of `(f i)`.

      initialize 4 id          == fromList [0,1,2,3]
      initialize 4 (\n -> n*n) == fromList [0,1,4,9]
      initialize 4 (always 0)  == fromList [0,0,0,0]
-}
initialize : Int -> (Int -> a) -> Array a
initialize = Native.Array.initialize

{-| Creates an array with a given length, filled with a default element.

      repeat 5 0     == fromList [0,0,0,0,0]
      repeat 3 "cat" == fromList ["cat","cat","cat"]

Notice that `repeat 3 x` is the same as `initialize 3 (always x)`.
-}
repeat : Int -> a -> Array a
repeat n e = initialize n (always e)

{-| Create an array from a list. -}
fromList : [a] -> Array a
fromList = Native.Array.fromList

{-| Create a list of elements from an array.

      toList (fromList [3,5,8]) == [3,5,8]
-}
toList : Array a -> [a]
toList = Native.Array.toList

-- TODO: make this a native function.
{-| Create an indexed list from an array. Each element of the array will be
paired with its index.

      toIndexedList (fromList ["cat","dog"]) == [(0,"cat"), (1,"dog")]
-}
toIndexedList : Array a -> [(Int, a)]
toIndexedList array =
    List.zip [ 0 .. Native.Array.length array - 1 ] (Native.Array.toList array)

{-| Apply a function on every element in an array.

      map sqrt (fromList [1,4,9]) == fromList [1,2,3]
-}
map : (a -> b) -> Array a -> Array b
map = Native.Array.map

{-| Apply a function on every element with its index as first argument.

      indexedMap (*) (fromList [5,5,5]) == fromList [0,5,10]
-}
indexedMap : (Int -> a -> b) -> Array a -> Array b
indexedMap = Native.Array.indexedMap

{-| Reduce an array from the left. Read `foldl` as &ldquo;fold from the left&rdquo;.

      foldl (::) [] (fromList [1,2,3]) == [3,2,1]
-}
foldl : (a -> b -> b) -> b -> Array a -> b
foldl = Native.Array.foldl

{-| Reduce an array from the right. Read `foldr` as &ldquo;fold from the right&rdquo;.

      foldr (+) 0 (repeat 3 5) == 15
-}
foldr : (a -> b -> b) -> b -> Array a -> b
foldr = Native.Array.foldr

{-| Keep only elements that satisfy the predicate:

      filter isEven (fromList [1..6]) == (fromList [2,4,6])
-}
filter : (a -> Bool) -> Array a -> Array a
filter isOkay arr =
    let update x xs = if isOkay x then Native.Array.push x xs else xs
    in
        Native.Array.foldl update Native.Array.empty arr

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
array length is unkown, use withDefaultGet oder getWithDefault.

      get 2 (A.fromList [3,2,1]) == 1
-}
get : Int -> Array a -> a
get = Native.Array.get

{-| Return Just the element at the index or Nothing if the index is out of range.

      getMaybe  2 (fromList [3,2,1]) == Just 2
      getMaybe  5 (fromList [3,2,1]) == Nothing
      getMaybe -1 (fromList [3,2,1]) == Nothing
-}
getMaybe : Int -> Array a -> Maybe a
getMaybe i array =
    if 0 <= i && i < Native.Array.length array
      then Just (Native.Array.get i array)
      else Nothing

{-| Get the element at the index. If the index is out of range, the given default
element is returned.

      getWithDefault 0 2 (fromList [3,2,1]) == 1
      getWithDefault 0 5 (fromList [3,2,1]) == 0
-}
getWithDefault : a -> Int -> Array a -> a
getWithDefault default i array =
    if 0 <= i && i < Native.Array.length array
      then Native.Array.get i array
      else default

{-| Set the element at a particular index. Returns an updated array.
If the index is out of range, the array is unaltered.

      set 1 7 (fromList [1,2,3]) == fromList [1,7,3]
-}
set : Int -> a -> Array a -> Array a
set = Native.Array.set

{-| Slice an array given a range. The selection is inclusive, so the last
element in the selection will also be in the new array. This may change in the
future. You can select from the end by giving a negative Int.

      slice  1  2 (fromList [0,1,2,3,4]) == fromList [1,2]
      slice  1 -2 (fromList [0,1,2,3,4]) == fromList [1,2,3]
      slice -3 -2 (fromList [0,1,2,3,4]) == fromList [2,3]
-}
slice : Int -> Int -> Array a -> Array a
slice = Native.Array.slice

{-| Return the length of an array.

      length (fromList [1,2,3]) == 3
-}
length : Array a -> Int
length = Native.Array.length

{-| Append two arrays to a new one.

      append (array 2 42) (array 3 81) == fromList [42,42,81,81,81]
-}
append : Array a -> Array a -> Array a
append = Native.Array.append
