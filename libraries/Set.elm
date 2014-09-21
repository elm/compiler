
module Set
    ( Set
    , empty, singleton, insert, remove
    , member
    , foldl, foldr, map
    , filter, partition
    , union, intersect, diff
    , toList, fromList
    ) where

{-| A set of unique values. The values can be any comparable type. This
includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or lists
of comparable types.

Insert, remove, and query operations all take *O(log n)* time.

# Build
@docs empty, singleton, insert, remove

# Query
@docs member

# Combine
@docs union, intersect, diff

# Lists
@docs toList, fromList

# Transform
@docs map, foldl, foldr, filter, partition

-}

import Maybe (Maybe)
import Dict as Dict
import List as List

type Set t = Dict.Dict t ()

{-| Create an empty set. -}
empty : Set comparable
empty = Dict.empty

{-| Create a set with one value. -}
singleton : comparable -> Set comparable
singleton k = Dict.singleton k ()

{-| Insert a value into a set. -}
insert : comparable -> Set comparable -> Set comparable
insert k = Dict.insert k ()

{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : comparable -> Set comparable -> Set comparable
remove = Dict.remove

{-| Determine if a value is in a set. -}
member : comparable -> Set comparable -> Bool
member = Dict.member

{-| Get the union of two sets. Keep all values. -}
union : Set comparable -> Set comparable -> Set comparable
union = Dict.union

{-| Get the intersection of two sets. Keeps values that appear in both sets. -}
intersect : Set comparable -> Set comparable -> Set comparable
intersect = Dict.intersect

{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set. -}
diff : Set comparable -> Set comparable -> Set comparable
diff = Dict.diff

{-| Convert a set into a list. -}
toList : Set comparable -> [comparable]
toList = Dict.keys

{-| Convert a list into a set, removing any duplicates. -}
fromList : [comparable] -> Set comparable
fromList xs = List.foldl insert empty xs

{-| Fold over the values in a set, in order from lowest to highest. -}
foldl : (comparable -> b -> b) -> b -> Set comparable -> b
foldl f b s = Dict.foldl (\k _ b -> f k b) b s

{-| Fold over the values in a set, in order from highest to lowest. -}
foldr : (comparable -> b -> b) -> b -> Set comparable -> b
foldr f b s = Dict.foldr (\k _ b -> f k b) b s

{-| Map a function onto a set, creating a new set with no duplicates. -}
map : (comparable -> comparable') -> Set comparable -> Set comparable'
map f s = fromList (List.map f (toList s))

{-| Create a new set consisting only of elements which satisfy a predicate. -}
filter : (comparable -> Bool) -> Set comparable -> Set comparable
filter p set = Dict.filter (\k _ -> p k) set

{-| Create two new sets; the first consisting of elements which satisfy a
predicate, the second consisting of elements which do not. -}
partition : (comparable -> Bool) -> Set comparable -> (Set comparable, Set comparable)
partition p set = Dict.partition (\k _ -> p k) set
