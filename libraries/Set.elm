
module Set (empty,singleton,insert,remove
           ,member
           ,foldl,foldr,map
           ,union,intersect,diff
           ,toList,fromList
           ) where

import Dict as Dict
import List as List

type Set t = Dict t ()

empty : Set t
empty = Dict.empty

singleton : t -> Set t
singleton k = Dict.singleton k ()

insert : t -> Set t -> Set t
insert k = Dict.insert k ()

remove : t -> Set t -> Set t
remove = Dict.remove

member : t -> Set t -> Bool
member = Dict.member

union : Set t -> Set t -> Set t
union = Dict.union

intersect : Set t -> Set t -> Set t
intersect = Dict.intersect

diff : Set t -> Set t -> Set t
diff = Dict.diff

toList : Set t -> [t]
toList = Dict.keys

fromList : [t] -> Set t
fromList xs = List.foldl (\k t -> Dict.insert k () t) empty xs

foldl : (a -> b -> b) -> b -> Set a -> b
foldl f b s = Dict.foldl (\k _ b -> f k b) b s

foldr : (a -> b -> b) -> b -> Set a -> b
foldr f b s = Dict.foldr (\k _ b -> f k b) b s

map : (a -> b) -> Set a -> Set b
map f s = fromList (List.map f (toList s))