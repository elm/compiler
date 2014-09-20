module Dict
    ( Dict
    , empty, singleton, insert, update
    , get, getOrElse, getOrFail
    , remove, member
    , filter
    , partition
    , foldl, foldr, map
    , union, intersect, diff
    , keys, values
    , toList, fromList
    ) where

{-| A dictionary mapping unique keys to values. The keys can be any comparable
type. This includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or
lists of comparable types.

Insert, remove, and query operations all take *O(log n)* time.

# Build
@docs empty, singleton, insert, update, remove

# Query
@docs member, get, getOrElse, getOrFail

# Combine
@docs union, intersect, diff

# Lists
@docs keys, values, toList, fromList

# Transform
@docs map, foldl, foldr, filter, partition

-}


import Basics (..)
import Maybe (..)
import Native.Error
import List
import List ((::))
import Native.Utils


-- BBlack and NBlack should only be used during the deletion
-- algorithm. Any other occurrence is a bug and should fail an assert.
type NColor
    = Red
    | Black
    | BBlack  -- Double Black, counts as 2 blacks for the invariant
    | NBlack  -- Negative Black, counts as -1 blacks for the invariant


showNColor : NColor -> String
showNColor c =
  case c of
    Red    -> "Red"
    Black  -> "Black"
    BBlack -> "BBlack"
    NBlack -> "NBlack"

type LeafColor
    = LBlack
    | LBBlack -- Double Black, counts as 2

showLColor : LeafColor -> String
showLColor c = case c of
  LBlack  -> "LBlack"
  LBBlack -> "LBBlack"

type Dict k v
    = RBNode NColor k v (Dict k v) (Dict k v)
    | RBEmpty LeafColor

{-| Create an empty dictionary. -}
empty : Dict comparable v
empty = RBEmpty LBlack

min : Dict k v -> (k,v)
min t =
  case t of
    RBNode _ k v (RBEmpty LBlack) _ -> (k,v)
    RBNode _ _ _ l _ -> min l
    RBEmpty LBlack -> Native.Error.raise "(min Empty) is not defined"

max : Dict k v -> (k, v)
max t =
  case t of
    RBNode _ k v _ (RBEmpty _) -> (k,v)
    RBNode _ _ _ _ r -> max r
    RBEmpty _ -> Native.Error.raise "(max Empty) is not defined"

{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

      animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

      get "Tom"   animals == Just Cat
      get "Mouse" animals == Just Mouse
      get "Spike" animals == Nothing

The `getOrElse` and `getOrFail` are built-in ways to handle common ways of
using the resulting `Maybe`.
-}
get : comparable -> Dict comparable v -> Maybe v
get k t =
 case t of
   RBEmpty LBlack -> Nothing
   RBNode _ k' v l r ->
    case Native.Utils.compare k k' of
      LT -> get k l
      EQ -> Just v
      GT -> get k r

{-| Get the value associated with a key. If the key is not found,
return a default value.

      animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

      getOrElse Dog "Tom"   animals == Cat
      getOrElse Dog "Mouse" animals == Mouse
      getOrElse Dog "Spike" animals == Dog
-}
getOrElse : v -> comparable -> Dict comparable v -> v
getOrElse base k t =
 case t of
   RBEmpty LBlack -> base
   RBNode _ k' v l r ->
    case Native.Utils.compare k k' of
      LT -> getOrElse base k l
      EQ -> v
      GT -> getOrElse base k r

{-| Get the value associated with a key.

      animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

      getOrFail "Tom"   animals == Cat
      getOrFail "Mouse" animals == Mouse
      getOrFail "Spike" animals == -- Runtime Error!

Warning: this function will result in a runtime error if the key is not found,
so it is best to use `get` or `getOrElse` unless you are sure the key will be
found.
-}
getOrFail : comparable -> Dict comparable v -> v
getOrFail k t =
 case t of
   RBEmpty LBlack -> Native.Error.raise "key not found when using 'getOrFail'"
   RBNode _ k' v l r ->
    case Native.Utils.compare k k' of
      LT -> getOrFail k l
      EQ -> v
      GT -> getOrFail k r

{-| Determine if a key is in a dictionary. -}
member : comparable -> Dict comparable v -> Bool
member k t = isJust <| get k t

ensureBlackRoot : Dict k v -> Dict k v
ensureBlackRoot t =
  case t of
    RBNode Red k v l r -> RBNode Black k v l r
    RBNode Black _ _ _ _ -> t
    RBEmpty LBlack -> t

{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision. -}
insert : comparable -> v -> Dict comparable v -> Dict comparable v
insert k v t = let u _ = Just v in 
  update k u t

{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made. -}
remove : comparable -> Dict comparable v -> Dict comparable v
remove k t = let u _ = Nothing in
  update k u t

type Flag = Insert | Remove | Same

showFlag : Flag -> String
showFlag f = case f of
  Insert -> "Insert"
  Remove -> "Remove"
  Same   -> "Same"

{-| Update the value of a dictionary for a specific key with a given function. -}
update : comparable -> (Maybe v -> Maybe v) -> Dict comparable v -> Dict comparable v
update k u t = 
  let up t = case t of
        RBEmpty LBlack -> case u Nothing of
          Nothing -> (Same, empty)
          Just v  -> (Insert, RBNode Red k v empty empty)
        RBNode c k' v l r -> case Native.Utils.compare k k' of
          EQ -> case u (Just v) of
            Nothing -> (Remove, rem c l r)
            Just v' -> (Same,   RBNode c k' v' l r)
          LT -> let (fl, l') = up l in
            case fl of
              Same   -> (Same, RBNode c k' v l' r)
              Insert -> (Insert, balance c k' v l' r)
              Remove -> (Remove, bubble c k' v l' r)
          GT -> let (fl, r') = up r in
            case fl of
              Same   -> (Same, RBNode c k' v l r')
              Insert -> (Insert, balance c k' v l r')
              Remove -> (Remove, bubble c k' v l r')
      (fl, t') = up t
  in case fl of
    Same   -> t'
    Insert -> ensureBlackRoot t'
    Remove -> blacken t'

{-| Create a dictionary with one key-value pair. -}
singleton : comparable -> v -> Dict comparable v
singleton k v = insert k v (RBEmpty LBlack)

isBBlack : Dict k v -> Bool
isBBlack t = case t of
  RBNode BBlack _ _ _ _ -> True
  RBEmpty LBBlack -> True
  _               -> False

moreBlack : NColor -> NColor
moreBlack c = case c of
  Black  -> BBlack
  Red    -> Black
  NBlack -> Red
  BBlack -> Native.Error.raise "Can't make a double black node more black!"

lessBlack : NColor -> NColor
lessBlack c = case c of
  BBlack -> Black
  Black  -> Red
  Red    -> NBlack
  NBlack -> Native.Error.raise "Can't make a negative black node less black!"

lessBlackTree : Dict k v -> Dict k v
lessBlackTree t = case t of
  RBNode c k v l r -> RBNode (lessBlack c) k v l r
  RBEmpty LBBlack -> RBEmpty LBlack

reportRemBug : String -> NColor -> String -> String -> a
reportRemBug msg c lgot rgot =
  Native.Error.raise <| List.concat [
    "Internal red-black tree invariant violated, expected ",
    msg,
    "and got",
    showNColor c,
    " ",
    lgot,
    " ",
    rgot,
    "\nPlease report this bug to https://github.com/elm-lang/Elm/issues"
    ]

-- Remove the top node from the tree, may leave behind BBlacks
rem : NColor -> Dict k v -> Dict k v -> Dict k v
rem c l r = case (l, r) of
  ((RBEmpty _), (RBEmpty _)) -> case c of
    Red   -> RBEmpty LBlack
    Black -> RBEmpty LBBlack
  ((RBEmpty cl), (RBNode cr k' v' l' r')) ->
    case (c, cl, cr) of
      (Black, LBlack, Red) -> RBNode Black k' v' l' r'
      _     -> reportRemBug "Black, LBlack, Red" c (showLColor cl) (showNColor cr)
  ((RBNode cl k' v' l' r'), (RBEmpty cr)) ->
    case (c, cl, cr) of
      (Black, Red, LBlack) -> RBNode Black k' v' l' r'
      _     -> reportRemBug "Black, Red, LBlack" c (showNColor cl) (showLColor cr)
  -- l and r are both RBNodes
  ((RBNode cl kl vl ll rl), (RBNode cr kr vr lr rr)) ->
    let l = RBNode cl kl vl ll rl
        r = RBNode cr kr vr lr rr
        (k, v) = max l
        l'     = remove_max cl kl vl ll rl
    in bubble c k v l' r

-- Kills a BBlack or moves it upward, may leave behind NBlack
bubble : NColor -> k -> v -> Dict k v -> Dict k v -> Dict k v
bubble c k v l r = if isBBlack l || isBBlack r
                   then balance (moreBlack c) k v (lessBlackTree l) (lessBlackTree r)
                   else RBNode c k v l r

-- Removes rightmost node, may leave root as BBlack
remove_max : NColor -> k -> v -> Dict k v -> Dict k v -> Dict k v
remove_max c k v l r = case r of
  RBEmpty _ -> rem c l r
  RBNode cr kr vr lr rr
    -> bubble c k v l (remove_max cr kr vr lr rr)

-- generalized tree balancing act
balance : NColor -> k -> v -> Dict k v -> Dict k v -> Dict k v
balance c k v l r =

  balance_node (RBNode c k v l r)

blackish : Dict k v -> Bool
blackish t = case t of
  RBNode c _ _ _ _ -> c == Black || c == BBlack
  RBEmpty _        -> True

balance_node : Dict k v -> Dict k v
balance_node t = 
  let assemble col xk xv yk yv zk zv a b c d = 
        RBNode (lessBlack col) yk yv (RBNode Black xk xv a b) (RBNode Black zk zv c d)
  in 
   if blackish t
   then case t of
     RBNode col zk zv (RBNode Red yk yv (RBNode Red xk xv a b) c) d ->
       assemble col xk xv yk yv zk zv a b c d
     RBNode col zk zv (RBNode Red xk xv a (RBNode Red yk yv b c)) d ->
       assemble col xk xv yk yv zk zv a b c d
     RBNode col xk xv a (RBNode Red zk zv (RBNode Red yk yv b c) d) ->
       assemble col xk xv yk yv zk zv a b c d
     RBNode col xk xv a (RBNode Red yk yv b (RBNode Red zk zv c d)) ->
       assemble col xk xv yk yv zk zv a b c d

     RBNode BBlack xk xv a (RBNode NBlack zk zv (RBNode Black yk yv b c) d) ->
       case d of
         (RBNode Black _ _ _ _) -> 
           RBNode Black yk yv (RBNode Black xk xv a b) (balance Black zk zv c (redden d))
         _ -> t

     RBNode BBlack zk zv (RBNode NBlack xk xv a (RBNode Black yk yv b c)) d ->
       case a of
         (RBNode Black _ _ _ _) -> 
           RBNode Black yk yv (balance Black xk xv (redden a) b) (RBNode Black zk zv c d)
         _ -> t
     _ -> t
   else t

-- make the top node black
blacken : Dict k v -> Dict k v
blacken t = case t of
  RBEmpty _ -> RBEmpty LBlack
  RBNode _ k v l r -> RBNode Black k v l r

-- make the top node red
redden : Dict k v -> Dict k v
redden t = case t of
  RBEmpty _ -> Native.Error.raise "can't make a Leaf red"
  RBNode _ k v l r -> RBNode Red k v l r

{-| Apply a function to all values in a dictionary. -}
map : (a -> b) -> Dict comparable a -> Dict comparable b
map f t =
  case t of
    RBEmpty LBlack -> RBEmpty LBlack
    RBNode c k v l r -> RBNode c k (f v) (map f l) (map f r)

{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key. -}
foldl : (comparable -> v -> b -> b) -> b -> Dict comparable v -> b
foldl f acc t =
  case t of
    RBEmpty LBlack -> acc
    RBNode _ k v l r -> foldl f (f k v (foldl f acc l)) r

{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key. -}
foldr : (comparable -> v -> b -> b) -> b -> Dict comparable v -> b
foldr f acc t =
  case t of
    RBEmpty LBlack -> acc
    RBNode _ k v l r -> foldr f (f k v (foldr f acc r)) l

{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary. -}
union : Dict comparable v -> Dict comparable v -> Dict comparable v
union t1 t2 = foldl insert t2 t1

{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary. -}
intersect : Dict comparable v -> Dict comparable v -> Dict comparable v
intersect t1 t2 = filter (\k _ -> k `member` t2) t1

{-| Keep a key-value pair when its key does not appear in the second dictionary.
Preference is given to the first dictionary. -}
diff : Dict comparable v -> Dict comparable v -> Dict comparable v
diff t1 t2 = foldl (\k v t -> remove k t) t1 t2

{-| Get all of the keys in a dictionary. -}
keys : Dict comparable v -> [comparable]
keys t   = foldr (\k v acc -> k :: acc) [] t

{-| Get all of the values in a dictionary. -}
values : Dict comparable v -> [v]
values t = foldr (\k v acc -> v :: acc) [] t

{-| Convert a dictionary into an association list of key-value pairs. -}
toList : Dict comparable v -> [(comparable,v)]
toList t = foldr (\k v acc -> (k,v) :: acc) [] t

{-| Convert an association list into a dictionary. -}
fromList : [(comparable,v)] -> Dict comparable v
fromList assocs = List.foldl (\(k,v) d -> insert k v d) empty assocs

{-| Keep a key-value pair when it satisfies a predicate. -}
filter : (comparable -> v -> Bool) -> Dict comparable v -> Dict comparable v
filter p dict =
  let add k v t = if p k v then insert k v t else t
  in  foldl add empty dict

{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition : (comparable -> v -> Bool) -> Dict comparable v -> (Dict comparable v, Dict comparable v)
partition p dict =
  let add k v (t1, t2) = if p k v
                            then (insert k v t1,  t2)
                            else (t1, insert k v t2)
  in  foldl add (empty, empty) dict
