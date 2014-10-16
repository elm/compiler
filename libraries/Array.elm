module Array where

{-| A library for fast immutable arrays. The elements in an array must have the
same type. The arrays are implemented in Relaxed Radix Balanced-Trees for fast
reads, updates, and appends.

# Creating Arrays
@docs empty, repeat, initialize, fromList

# Basics
@docs length, push, append

# Get and Set
@docs get, getOrElse, getOrFail, set

# Taking Arrays Apart
@docs slice, toList, toIndexedList

# Mapping and Folding
@docs map, indexedMap, foldl, foldr
-}

import Native.Array
import Basics (..)
import Maybe (..)
import List

type Array a = Array

{-| Initialize an array. `initialize n f` creates an array of length `n` with
the element at index `i` initialized to the result of `(f i)`.

      initialize 4 identity    == fromList [0,1,2,3]
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

{-| Get the element at a particular index.

      getOrFail 0 (A.fromList [0,1,2]) == 0

Warning: this function will result in a runtime error if the index is not found,
so it is best to use `get` or `getOrElse` unless you are sure the index will be
found.
-}
getOrFail : Int -> Array a -> a
getOrFail = Native.Array.get

{-| Return Just the element at the index or Nothing if the index is out of range.

      get  0 (fromList [0,1,2]) == Just 0
      get  2 (fromList [0,1,2]) == Just 2
      get  5 (fromList [0,1,2]) == Nothing
      get -1 (fromList [0,1,2]) == Nothing
-}
get : Int -> Array a -> Maybe a
get i array =
    if 0 <= i && i < Native.Array.length array
      then Just (Native.Array.get i array)
      else Nothing

{-| Get the element at the index. Or if the index is out of range, a default
value is returned.

      getOrElse 0 2 (fromList [0,1,2]) == 2
      getOrElse 0 5 (fromList [0,1,2]) == 0
-}
getOrElse : a -> Int -> Array a -> a
getOrElse default i array =
    if 0 <= i && i < Native.Array.length array
      then Native.Array.get i array
      else default

{-| Set the element at a particular index. Returns an updated array.
If the index is out of range, the array is unaltered.

      set 1 7 (fromList [1,2,3]) == fromList [1,7,3]
-}
set : Int -> a -> Array a -> Array a
set = Native.Array.set

{-| Get a sub-section of an array: `(slice start end array)`. The `start` is a
zero-based index where we will start our slice. The `end` is a zero-based index
that indicates the end of the slice. The slice extracts up to but not including
`end`.

      slice  0  3 (fromList [0,1,2,3,4]) == fromList [0,1,2]
      slice  1  4 (fromList [0,1,2,3,4]) == fromList [1,2,3]

Both the `start` and `end` indexes can be negative, indicating an offset from
the end of the array.

      slice  1 -1 (fromList [0,1,2,3,4]) == fromList [1,2,3]
      slice -2  5 (fromList [0,1,2,3,4]) == fromList [3,4]

This makes it pretty easy to `pop` the last element off of an array: `slice 0 -1 array`
-}
slice : Int -> Int -> Array a -> Array a
slice = Native.Array.slice

{-| Return the length of an array.

      length (fromList [1,2,3]) == 3
-}
length : Array a -> Int
length = Native.Array.length

{-| Append two arrays to a new one.

      append (repeat 2 42) (repeat 3 81) == fromList [42,42,81,81,81]
-}
append : Array a -> Array a -> Array a
append = Native.Array.append
