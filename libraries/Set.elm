
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
empty : Set t
empty = Dict.empty

-- Create a set with one value.
singleton : t -> Set t
singleton k = Dict.singleton k ()

-- Insert a value into a set.
insert : t -> Set t -> Set t
insert k = Dict.insert k ()

-- Remove a value from a set. If the value is not found, no changes are made.
remove : t -> Set t -> Set t
remove = Dict.remove

-- Determine if a value is in a set.
member : t -> Set t -> Bool
member = Dict.member

-- Get the union of two sets. Keep all values.
union : Set t -> Set t -> Set t
union = Dict.union

-- Get the intersection of two sets. Keeps values that appear in both sets.
intersect : Set t -> Set t -> Set t
intersect = Dict.intersect

-- Get the difference between the first set and the second. Keeps values that do not appear in the second set.
diff : Set t -> Set t -> Set t
diff = Dict.diff

-- Convert a set into a list.
toList : Set t -> [t]
toList = Dict.keys

-- Convert a list into a set, removing any duplicates.
fromList : [t] -> Set t
fromList xs = List.foldl (\k t -> Dict.insert k () t) empty xs

-- Fold over the values in a set, in order from lowest to highest.
foldl : (a -> b -> b) -> b -> Set a -> b
foldl f b s = Dict.foldl (\k _ b -> f k b) b s

-- Fold over the values in a set, in order from highest to lowest.
foldr : (a -> b -> b) -> b -> Set a -> b
foldr f b s = Dict.foldr (\k _ b -> f k b) b s

-- Map a function onto a set, creating a new set with no duplicates.
map : (a -> b) -> Set a -> Set b
map f s = fromList (List.map f (toList s))
