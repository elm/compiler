
module Set (empty,singleton,insert,remove
           ,member
           ,foldl,foldr,map
           ,union,intersect,diff
           ,toList,fromList
           ) where

empty = Dict.empty
singleton k = Dict.singleton k ()
insert k = Dict.insert k ()
remove = Dict.remove
member = Dict.member

union = Dict.union
intersect = Dict.intersect
diff = Dict.diff

toList = Dict.keys
fromList = List.foldl (\k t -> Dict.insert k () t) empty

foldl f = Dict.foldl (\k v b -> f k b)
foldr f = Dict.foldr (\k v b -> f k b)
map f t = fromList . List.map f $ toList t