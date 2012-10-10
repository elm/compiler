
module Set (empty,singleton,insert,remove,member) where

import Map as Map

empty = Map.empty
singleton k = Map.singleton k ()
insert k = Map.insert k ()
remove = Map.remove
member = Map.member
