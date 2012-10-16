
module Set (empty,singleton,insert,remove,member) where

import Dict as Dict

empty = Dict.empty
singleton k = Dict.singleton k ()
insert k = Dict.insert k ()
remove = Dict.remove
member = Dict.member
