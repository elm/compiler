module List where

{-| A library for manipulating lists of values. Every value in a
list must have the same type.

# Basics
@docs (::), (++), isEmpty, length, reverse, map

# Sub-lists
@docs head, tail, last, filter, take, drop

# Putting Lists Together
@docs concat, join, intersperse, zip, zipWith, repeat

# Taking Lists Apart
@docs partition, unzip

# Folds
@docs foldr, foldl

# Special Maps
@docs filterMap, concatMap, indexedMap

# Special Folds
@docs sum, product, maximum, minimum, all, any, foldr1, foldl1, scanl, scanl1

# Sorting
@docs sort, sortBy, sortWith

# Additional Zips
@docs zip3, zip4, zip5, zipWith3, zipWith4, zipWith5

If you can think of a legitimate use of `zipN` or `zipWithN` where `N` is 6 or
more, please let us know on [the
list](https://groups.google.com/forum/#!forum/elm-discuss). The current
sentiment is that it is already quite error prone once you get to 4 and
possibly should be approached another way.
-}

import Basics (..)
import Maybe ( Maybe(Just,Nothing) )
import Native.List

{-| Add an element to the front of a list `(1 :: [2,3] == [1,2,3])` -}
(::) : a -> [a] -> [a]
(::) = Native.List.cons

{-| Puts two appendable things together:

      [1,1] ++ [2,3] == [1,1,2,3]
      "abc" ++ "123" == "abc123"
-}
(++) : appendable -> appendable -> appendable
(++) = Native.List.append

infixr 5 ::
infixr 5 ++

{-| Extract the first element of a list. List must be non-empty.
`(head [1,2,3] == 1)`
-}
head : [a] -> a
head = Native.List.head

{-| Extract the elements after the head of the list. List must be non-empty.
`(tail [1,2,3] == [2,3])`
-}
tail : [a] -> [a]
tail = Native.List.tail

{-| Extract the last element of a list. List must be non-empty.
`(last [1,2,3] == 3)`
-}
last : [a] -> a
last = Native.List.last

{-| Check if a list is empty `(isEmpty [] == True)` -}
isEmpty : [a] -> Bool
isEmpty xs =
    case xs of
      [] -> True
      _  -> False

{-| Apply a function to every element of a list: `(map sqrt [1,4,9] == [1,2,3])` -}
map : (a -> b) -> [a] -> [b]
map = Native.List.map

{-| Same as `map` but the function is also applied to the index of each
element (starting at zero).

      indexedMap (,) ["Tom","Sue","Bob"] == [ (0,"Tom"), (1,"Sue"), (2,"Bob") ]
-}
indexedMap : (Int -> a -> b) -> [a] -> [b]
indexedMap f xs =
    zipWith f [ 0 .. length xs - 1 ] xs

{-| Reduce a list from the left: `(foldl (::) [] [1,2,3] == [3,2,1])` -}
foldl : (a -> b -> b) -> b -> [a] -> b
foldl = Native.List.foldl

{-| Reduce a list from the right: `(foldr (+) 0 [1,2,3] == 6)` -}
foldr : (a -> b -> b) -> b -> [a] -> b
foldr = Native.List.foldr

{-| Reduce a list from the left without a base case. List must be non-empty. -}
foldl1 : (a -> a -> a) -> [a] -> a
foldl1 = Native.List.foldl1

{-| Reduce a list from the right without a base case. List must be non-empty. -}
foldr1 : (a -> a -> a) -> [a] -> a
foldr1 = Native.List.foldr1

{-| Reduce a list from the left, building up all of the intermediate results into a list.

      scanl (+) 0 [1,2,3,4] == [0,1,3,6,10]
-}
scanl : (a -> b -> b) -> b -> [a] -> [b]
scanl = Native.List.scanl

{-| Same as scanl but it doesn't require a base case. List must be non-empty.

      scanl1 (+) [1,2,3,4] == [1,3,6,10]
-}
scanl1 : (a -> a -> a) -> [a] -> [a]
scanl1 = Native.List.scanl1

{-| Keep only elements that satisfy the predicate:
`(filter isEven [1..6] == [2,4,6])`
-}
filter : (a -> Bool) -> [a] -> [a]
filter = Native.List.filter

{-| Apply a function that may succeed to all values in the list, but only keep
the successes.

      toInt : String -> Maybe Int

      filterMap toInt ["3", "4.0", "5", "hats"] == [3,5]
-}
filterMap : (a -> Maybe b) -> [a] -> [b]
filterMap f xs = foldr (maybeCons f) [] xs

maybeCons : (a -> Maybe b) -> a -> [b] -> [b]
maybeCons f mx xs =
    case f mx of
      Just x -> x :: xs
      Nothing -> xs

{-| Determine the length of a list: `(length [1,2,3] == 3)` -}
length : [a] -> Int
length = Native.List.length

{-| Reverse a list. `(reverse [1..4] == [4,3,2,1])` -}
reverse : [a] -> [a]
reverse = Native.List.reverse

{-| Check to see if all elements satisfy the predicate. -}
all : (a -> Bool) -> [a] -> Bool
all = Native.List.all

{-| Check to see if any elements satisfy the predicate. -}
any : (a -> Bool) -> [a] -> Bool
any = Native.List.any

{-| Concatenate a list of appendable things:

      concat [[1,2],[3],[4,5]] == [1,2,3,4,5]
      concat ["tree","house"]  == "treehouse"
-}
concat : [appendable] -> appendable
concat = Native.List.concat

{-| Map a given function onto a list and flatten the resulting lists.

      concatMap f xs == concat (map f xs)
-}
concatMap : (a -> appendable) -> [a] -> appendable
concatMap f list = concat (map f list)

{-| Get the sum of the list elements. `(sum [1..4] == 10)` -}
sum : [number] -> number
sum = foldl (+) 0

{-| Get the product of the list elements. `(product [1..4] == 24)` -}
product : [number] -> number
product = foldl (*) 1

{-| Find the maximum element in a non-empty list: `maximum [1,4,2] == 4` -}
maximum : [comparable] -> comparable
maximum = foldl1 max

{-| Find the minimum element in a non-empty list: `minimum [3,2,1] == 1` -}
minimum : [comparable] -> comparable
minimum = foldl1 min

{-| Partition a list based on a predicate. The first list contains all values
that satisfy the predicate, and the second list contains all the value that do
not.

      partition (\x -> x < 3) [0..5] == ([0,1,2], [3,4,5])
      partition isEven        [0..5] == ([0,2,4], [1,3,5])
-}
partition : (a -> Bool) -> [a] -> ([a],[a])
partition pred =
    let step x (ts, fs) = if pred x
                          then (x::ts, fs)
                          else (ts, x::fs)
    in foldr step ([],[])

{-| Combine two lists, combining them into tuples pairwise.
If one list is longer, the extra elements are dropped.

      zip [1,2,3] [6,7] == [(1,6),(2,7)]
      zip == zipWith (,)
-}
zip : [a] -> [b] -> [(a,b)]
zip = Native.List.zip

zip3 : [a] -> [b] -> [c] -> [(a,b,c)]
zip3 = Native.List.zipWith3 (,,)

zip4 : [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4 = Native.List.zipWith4 (,,,)

zip5 : [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
zip5 = Native.List.zipWith5 (,,,,)

{-| Combine two lists, combining them with the given function.
If one list is longer, the extra elements are dropped.

      zipWith (+) [1,2,3] [1,2,3,4] == [2,4,6]
-}
zipWith : (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = Native.List.zipWith

zipWith3 : (a -> b -> c -> x) -> [a] -> [b] -> [c] -> [x]
zipWith3 = Native.List.zipWith3

zipWith4 : (a -> b -> c -> d -> x) -> [a] -> [b] -> [c] -> [d] -> [x]
zipWith4 = Native.List.zipWith4

zipWith5 : (a -> b -> c -> d -> e -> x) -> [a] -> [b] -> [c] -> [d] -> [e] -> [x]
zipWith5 = Native.List.zipWith5

{-| Decompose a list of tuples. 

      unzip [(0, True), (17, False), (1337, True)] == ([0,17,1337], [True,False,True])
-}
unzip : [(a,b)] -> ([a],[b])
unzip =
    let step (x,y) (xs, ys) = (x::xs, y::ys)
    in foldr step ([], [])

{-| Places the given value between all of the lists in the second
argument and concatenates the result.

      join "a" ["H","w","ii","n"] == "Hawaiian"
-}
join : appendable -> [appendable] -> appendable
join = Native.List.join

{-| Places the given value between all members of the given list.

      intersperse "on" ["turtles","turtles","turtles"] == ["turtles","on","turtles","on","turtles"]
-}
intersperse : a -> [a] -> [a]
intersperse sep xs =
    case xs of
      [] -> []
      hd::tl ->
          let step x rest = sep :: x :: rest
              spersed = foldr step [] tl
          in hd :: spersed

{-| Take the first n members of a list: `(take 2 [1,2,3,4] == [1,2])` -}
take : Int -> [a] -> [a]
take = Native.List.take

{-| Drop the first n members of a list: `(drop 2 [1,2,3,4] == [3,4])` -}
drop : Int -> [a] -> [a]
drop = Native.List.drop

{-| Creates a list with *n* copies of a value:
`(repeat 3 (0,0) == [(0,0),(0,0),(0,0)]`
-}
repeat : Int -> a -> [a]
repeat = Native.List.repeat

{-| Sort values from lowest to highest: `sort [3,1,5] == [1,3,5]`
-}
sort : [comparable] -> [comparable]
sort = Native.List.sort

{-| Sort values by a derived property.

```haskell
alice = { name="Alice", height=1.62 }
bob   = { name="Bob"  , height=1.85 }
chuck = { name="Chuck", height=1.76 }

sortBy .name   [chuck,alice,bob] == [alice,bob,chuck]
sortBy .height [chuck,alice,bob] == [alice,chuck,bob]

sortBy String.length ["mouse","cat"] == ["cat","mouse"]
```
-}
sortBy : (a -> comparable) ->  [a] -> [a]
sortBy = Native.List.sortBy

{-| Sort values with a custom comparison function.

```haskell
sortWith flippedComparison [1..5] == [5,4,3,2,1]

flippedComparison a b =
     case compare a b of
       LT -> GT
       EQ -> EQ
       GT -> LT
```

This is also the most general sort function, allowing you
to define any other: `sort == sortWith compare`
-}
sortWith : (a -> a -> Order) ->  [a] -> [a]
sortWith = Native.List.sortWith
