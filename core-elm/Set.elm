
module Set (empty,singleton,insert,remove
           ,member
           ,fold,map
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

fold f = Dict.fold (\k v b -> f k b)
map f t = fromList . List.map f $ toList t