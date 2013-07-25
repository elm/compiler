
module List where

import Native.Utils
import Native.List as Native

-- Add an element to the front of a list `(1 :: [2,3] == [1,2,3])`
(::) : a -> [a] -> [a]
(::) = Native.cons

-- Puts two appendable things together:
--
--       [1,1] ++ [2,3] == [1,1,2,3]
--       "abc" ++ "123" == "abc123"
(++) : appendable -> appendable -> appendable
(++) = Native.append

-- Extract the first element of a list. List must be non-empty.
-- `(head [1,2,3] == 1)`
head : [a] -> a
head = Native.head

-- Extract the elements after the head of the list. List must be non-empty.
--  `(tail [1,2,3] == [2,3])`
tail : [a] -> [a]
tail = Native.tail

-- Extract the last element of a list. List must be non-empty.
-- `(last [1,2,3] == 3)`
last : [a] -> a
last = Native.last

-- Check if a list is empty `(isEmpty [] == True)`
isEmpty : [a] -> Bool
isEmpty xs =
    case xs of
      [] -> True
      _  -> False

-- Apply a function to every element of a list: `(map sqrt [1,4,9] == [1,2,3])`
map : (a -> b) -> [a] -> [b]
map = Native.map

-- Reduce a list from the left: `(foldl (::) [] "gateman" == "nametag")`
foldl : (a -> b -> b) -> b -> [a] -> b
foldl = Native.foldl

-- Reduce a list from the right: `(foldr (+) 0 [1,2,3] == 6)`
foldr : (a -> b -> b) -> b -> [a] -> b
foldr = Native.foldr

-- Reduce a list from the left without a base case. List must be non-empty.
foldl1 : (a -> a -> a) -> [a] -> a
foldl1 = Native.foldl1

-- Reduce a list from the right without a base case. List must be non-empty.
foldr1 : (a -> a -> a) -> [a] -> a
foldr1 = Native.foldr1

-- Reduce a list from the left, building up all of the intermediate results into a list.
--
--       scanl (+) 0 [1,2,3,4] == [0,1,3,6,10]
scanl : (a -> b -> b) -> b -> [a] -> [b]
scanl = Native.scanl

-- Same as scanl but it doesn't require a base case. List must be non-empty.
--
--       scanl1 (+) [1,2,3,4] == [1,3,6,10]
scanl1 : (a -> a -> a) -> [a] -> [a]
scanl1 = Native.scanl1

-- Filter out elements which do not satisfy the predicate: `(filter isLower "AaBbCc" == "abc")`
filter : (a -> Bool) -> [a] -> [a]
filter = Native.filter

-- Determine the length of a list: `(length "innumerable" == 11)`
length : [a] -> Int
length = Native.length

-- Reverse a list. `(reverse [1..4] == [4,3,2,1])`
reverse : [a] -> [a]
reverse = Native.reverse

-- Check to see if all elements satisfy the predicate.
all : (a -> Bool) -> [a] -> Bool
all = Native.all

-- Check to see if any elements satisfy the predicate.
any : (a -> Bool) -> [a] -> Bool
any = Native.any

-- Check to see if all elements are True.
and : [Bool] -> Bool
and = Native.and

-- Check to see if any elements are True.
or : [Bool] -> Bool
or = Native.or

-- Concatenate a list of appendable things:
--
--       concat ["tree","house"] == "treehouse"
concat : [appendable] -> appendable
concat = Native.concat

-- Map a given function onto a list and flatten the resulting lists.
--
--       concatMap f xs == concat (map f xs)
concatMap : (a -> appendable) -> [a] -> appendable
concatMap f list = concat (map f list)

-- Get the sum of the list elements. `(sum [1..4] == 10)`
sum : [number] -> number
sum = foldl (+) 0

-- Get the product of the list elements. `(product [1..4] == 24)`
product : [number] -> number
product = foldl (*) 1

-- Find the maximum element in a non-empty list: `maximum [1,4,2] == 4`
maximum : [comparable] -> comparable
maximum = foldl1 Native.Utils.max

-- Find the minimum element in a non-empty list: `minimum [3,2,1] == 1`
minimum : [comparable] -> comparable
minimum = foldl1 Native.Utils.min

-- Split a list based on the predicate.
partition : (a -> Bool) -> [a] -> ([a],[a])
partition pred lst =
    case lst of
      []    -> ([],[])
      x::xs -> let (bs,cs) = partition pred xs in
               if pred x then (x::bs,cs) else (bs,x::cs)

-- Combine two lists, combining them into tuples pairwise.
-- If one list is longer, the extra elements are dropped.
--
--       zip [1,2,3] [6,7] == [(1,6),(2,7)]
--       zip == zipWith (,)
zip : [a] -> [b] -> [(a,b)]
zip = Native.zip

-- Combine two lists, combining them with the given function.
-- If one list is longer, the extra elements are dropped.
--
--       zipWith (+) [1,2,3] [1,2,3,4] == [2,4,6]
zipWith : (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = Native.zipWith

-- Decompose a list of tuples.
unzip : [(a,b)] -> ([a],[b])
unzip pairs =
  case pairs of
    []        -> ([],[])
    (x,y)::ps -> let (xs,ys) = (unzip ps) in (x::xs,y::ys)

-- Split a list with a given seperator.
--
--       split "," "hello,there,friend" == ["hello", "there", "friend"]
split : [a] -> [a] -> [[a]]
split = Native.split

-- Places the given value between all of the lists in the second argument
-- and concatenates the result. 
--
--       join "a" ["H","w","ii","n"] == "Hawaiian"
join : appendable -> [appendable] -> appendable
join = Native.join

-- Places the given value between all members of the given list.
--
--       intersperse ' ' "INCEPTION" == "I N C E P T I O N"
intersperse : a -> [a] -> [a]
intersperse sep xs =
  case xs of 
    a::b::cs -> a :: sep :: intersperse sep (b::cs)
    [a] -> [a]
    []  -> []

-- Take the first n members of a list: `(take 2 [1,2,3,4] == [1,2])`
take : Int -> [a] -> [a]
take = Native.take

-- Drop the first n members of a list: `(drop 2 [1,2,3,4] == [3,4])`
drop : Int -> [a] -> [a]
drop = Native.drop
