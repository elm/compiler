module List where

import Native.Utils (min, max)
import Native.List as L

-- Extract the first element of a list. List must be non-empty.
head : [a] -> a

-- Extract the elements after the head of the list. List must be non-empty.
tail : [a] -> [a]

-- Extract the last element of a list. List must be non-empty.
last : [a] -> a

-- Apply a function to every element of a list.
map  : (a -> b) -> [a] -> [b]

-- Reduce a list from the left.
foldl  : (a -> b -> b) -> b -> [a] -> b

-- Reduce a list from the right.
foldr  : (a -> b -> b) -> b -> [a] -> b

-- Reduce a list from the left without a base case. List must be non-empty.
foldl1 : (a -> a -> a) -> [a] -> a

-- Reduce a list from the right without a base case. List must be non-empty.
foldr1 : (a -> a -> a) -> [a] -> a

-- Reduce a list from the left, building up all of the intermediate results into a list.
scanl  : (a -> b -> b) -> b -> [a] -> [b]

-- Same as scanl but it doesn't require a base case. List must be non-empty.
scanl1 : (a -> a -> a) -> [a] -> [a]

-- Filter out elements which do not satisfy the predicate.
filter  : (a -> Bool) -> [a] -> [a]

-- Determine the length of a list.
length  : [a] -> Int

-- Reverse a list.
reverse : [a] -> [a]

-- Check to see if all elements satisfy the predicate.
all : (a -> Bool) -> [a] -> Bool

-- Check to see if any elements satisfy the predicate.
any : (a -> Bool) -> [a] -> Bool

-- Check to see if all elements are True.
and : [Bool] -> Bool

-- Check to see if any elements are True.
or  : [Bool] -> Bool

-- Flatten a list of lists.
concat : [[a]] -> [a]

-- Map a given function onto a list and flatten the resulting lists. (concatMap f xs == concat (map f xs))
concatMap : (a -> [b]) -> [a] -> [b]
concatMap f = L.concat . L.map f

-- Get the sum of the list elements.
sum = L.foldl (+) 0

-- Get the product of the list elements.
product = L.foldl (*) 1

-- Find the highest number in a non-empty list.
maximum = L.foldl1 max

-- Find the lowest number in a non-empty list.
minimum = L.foldl1 min

-- Split a list based on the predicate.
partition : (a -> Bool) -> [a] -> ([a],[a])
partition pred lst =
    case lst of
      []    -> ([],[])
      x::xs -> let (bs,cs) = partition pred xs in
               if pred x then (x::bs,cs) else (bs,x::cs)

-- Combine two lists, combining them into tuples pairwise. If one input list has extra elements (it is longer), those elements are dropped.
zip : [a] -> [b] -> [(a,b)]

-- Combine two lists, combining them with the given function. If one input list has extra elements (it is longer), those elements are dropped.
zipWith : (a -> b -> c) -> [a] -> [b] -> [c]

-- Decompose a list of tuples
unzip : [(a,b)] -> ([a],[b])
unzip pairs =
  case pairs of
    []        -> ([],[])
    (x,y)::ps -> let (xs,ys) = (unzip ps) in (x::xs,y::ys)

-- Split a list
--     split "," "hello,there,friend" == ["hello", "there", "friend"]
split : [a] -> [a] -> [[a]]

-- Places the given value between all of the lists in the second argument and concatenates the result. 
--     join xs xss = concat (intersperse xs xss)
join  : [a] -> [[a]] -> [a]

-- Places the given value between all members of the given list.
intersperse : a -> [a] -> [a]
intersperse sep xs =
  case xs of 
    a::b::cs -> a :: sep :: intersperse sep (b::cs)
    [a] -> [a]
    []  -> []

-- Add an element to the front of a list 
--     a :: [b,c] = [a,b,c]
(::) : a -> [a] -> [a]
-- :: has no direct implementation: Handled by Parser as a binop and converted to a Cons function

-- Appends two lists.
(++) : [a] -> [a] -> [a]

-- Take the first n members of a list.
--     take 2 [1,2,3,4]) ==> [1,2]
take : Int -> [a] -> [a]

-- Drop the first n members of a list. 
--     drop 2 [1,2,3,4]) ==> [3,4]
drop : Int -> [a] -> [a]

