
module Set (empty,singleton,insert,remove
           ,member
           ,foldl,foldr,map
           ,union,intersect,diff
           ,toList,fromList
           ) where

import Dict as Dict
import List as List

type Set t = Dict t ()

-- Create an empty set.
empty : Set (Comparable k)
empty = Dict.empty

-- Create a set with one value.
singleton : Comparable k -> Set (Comparable k)
singleton k = Dict.singleton k ()

-- Insert a value into a set.
insert : Comparable k -> Set (Comparable k) -> Set (Comparable k)
insert k = Dict.insert k ()

-- Remove a value from a set. If the value is not found, no changes are made.
remove : Comparable k -> Set (Comparable k) -> Set (Comparable k)
remove = Dict.remove

-- Determine if a value is in a set.
member : Comparable k -> Set (Comparable k) -> Bool
member = Dict.member

-- Get the union of two sets. Keep all values.
union : Set (Comparable k) -> Set (Comparable k) -> Set (Comparable k)
union = Dict.union

-- Get the intersection of two sets. Keeps values that appear in both sets.
intersect : Set (Comparable k) -> Set (Comparable k) -> Set (Comparable k)
intersect = Dict.intersect

-- Get the difference between the first set and the second. Keeps values
-- that do not appear in the second set.
diff : Set (Comparable k) -> Set (Comparable k) -> Set (Comparable k)
diff s1 s2 = foldl remove s1 s2

-- Convert a set into a list.
toList : Set (Comparable k) -> [Comparable k]
toList = Dict.keys

-- Convert a list into a set, removing any duplicates.
fromList : [Comparable k] -> Set (Comparable k)
fromList xs = List.foldl insert empty xs

-- Fold over the values in a set, in order from lowest to highest.
foldl : (Comparable a -> b -> b) -> b -> Set (Comparable a) -> b
foldl f b s = Dict.foldl (\k _ b -> f k b) b s

-- Fold over the values in a set, in order from highest to lowest.
foldr : (Comparable a -> b -> b) -> b -> Set (Comparable a) -> b
foldr f b s = Dict.foldr (\k _ b -> f k b) b s

-- Map a function onto a set, creating a new set with no duplicates.
map : (Comparable a -> Comparable b) -> Set (Comparable a) -> Set (Comparable b)
map f s = fromList (List.map f (toList s))
