
module Dict (empty,singleton,insert
            ,lookup,findWithDefault
            ,remove,member
            ,foldl,foldr,map
            ,union,intersect,diff
            ,keys,values
            ,toList,fromList
            ) where

import Maybe as Maybe
import Native.Error as Error
import List as List
import Native.Utils (compare)

data NColor = Red | Black

data Dict k v = RBNode NColor k v (Dict k v) (Dict k v) | RBEmpty

-- Create an empty dictionary.
empty : Dict (Comparable k) v
empty = RBEmpty

{-- Helpers for checking invariants

-- Check that the tree has an equal number of black nodes on each path
equal_pathLen t =
  let path_numBlacks t =
     case t of
       RBEmpty -> 1
       RBNode col _ _ l r ->
          let { bl = path_numBlacks l ; br = path_numBlacks r } in
          if bl /= br || bl == 0-1 || br == 0-1
              then 0-1
              else bl + (if col == Red then 0 else 1)
  in 0-1 /= path_numBlacks t

rootBlack t =
  case t of
    RBEmpty -> True
    RBNode Black _ _ _ _ -> True
    _ -> False

redBlack_children t =
  case t of
  { RBNode Red _ _ (RBNode Red _ _ _ _) _ -> False
  ; RBNode Red _ _ _ (RBNode Red _ _ _ _) -> False
  ; RBEmpty -> True
  ; RBNode _ _ _ l r -> redBlack_children l && redBlack_children r
  }

findExtreme f t =
  case t of
  { RBEmpty -> Nothing
  ; RBNode c k _ l r ->
      case findExtreme f (f (l,r)) of
      { Nothing -> Just k
      ; Just k' -> Just k' }
  }

findminRbt t = findExtreme fst t
findmaxRbt t = findExtreme snd t

-- "Option LT than"
-- Returns True if either xo or yo is Nothing
-- Otherwise returns the result of comparing the values using f

optionRelation f u xo yo =
  case (xo,yo) of
  { (Nothing,_) -> u
  ; (_,Nothing) -> u
  ; (Just x, Just y) -> f x y }

olt  xo yo = optionRelation (< ) True xo yo
olte xo yo = optionRelation (<=) True xo yo

ordered t =
  case t of
  { RBEmpty -> True
  ; RBNode c k v l r ->
      let (lmax,rmin) = (findmaxRbt l, findminRbt r) in
      olte lmax (Just k) && olte (Just k) rmin && ordered l && ordered r
  }

-- Check that there aren't any right red nodes in the tree *)
leftLeaning t =
  case t of
  { RBEmpty -> True
  ; RBNode _ _ _ (RBNode Black _ _ _ _) (RBNode Red _ _ _ _) -> False
  ; RBNode _ _ _ RBEmpty (RBNode Red _ _ _ _) -> False
  ; RBNode _ _ _ l r -> (leftLeaning l) && (leftLeaning r)
  }

invariants_hold t =
  ordered t && rootBlack t && redBlack_children t &&
  equal_pathLen t && leftLeaning t

--** End invariant helpers *****
--}


min : Dict k v -> (k,v)
min t =
  case t of
    RBNode _ k v RBEmpty _ -> (k,v)
    RBNode _ _ _ l _ -> min l
    RBEmpty -> Error.raise "(min Empty) is not defined"

{--
max t =
  case t of
  { RBNode _ k v _ RBEmpty -> (k,v)
  ; RBNode _ _ _ _ r -> max r
  ; RBEmpty -> Error.raise "(max Empty) is not defined"
  }
--}

-- Lookup the value associated with a key.
lookup : Comparable k -> Dict (Comparable k) v -> Maybe v
lookup k t =
 case t of
   RBEmpty -> Maybe.Nothing
   RBNode _ k' v l r ->
    case compare k k' of
      LT -> lookup k l
      EQ -> Maybe.Just v
      GT -> lookup k r

-- Find the value associated with a key. If the key is not found,
-- return the default value.
findWithDefault : v -> Comparable k -> Dict (Comparable k) v -> v
findWithDefault base k t =
 case t of
   RBEmpty -> base
   RBNode _ k' v l r ->
    case compare k k' of
      LT -> findWithDefault base k l
      EQ -> v
      GT -> findWithDefault base k r

{--
-- Find the value associated with a key. If the key is not found, there will be a runtime error.
find k t =
 case t of
 { RBEmpty -> Error.raise "Key was not found in dictionary!"
 ; RBNode _ k' v l r ->
    case compare k k' of
    { LT -> find k l
    ; EQ -> v
    ; GT -> find k r }
 }
--}

-- Determine if a key is in a dictionary.
member : Comparable k -> Dict (Comparable k) v -> Bool
-- Does t contain k?
member k t = Maybe.isJust $ lookup k t

rotateLeft : Dict k v -> Dict k v
rotateLeft t =
 case t of
   RBNode cy ky vy a (RBNode cz kz vz b c) -> RBNode cy kz vz (RBNode Red ky vy a b) c
   _ -> Error.raise "rotateLeft of a node without enough children"

-- rotateRight -- the reverse, and
-- makes Y have Z's color, and makes Z Red.
rotateRight : Dict k v -> Dict k v
rotateRight t =
 case t of
   RBNode cz kz vz (RBNode cy ky vy a b) c -> RBNode cz ky vy a (RBNode Red kz vz b c)
   _ -> Error.raise "rotateRight of a node without enough children"

rotateLeftIfNeeded : Dict k v -> Dict k v
rotateLeftIfNeeded t =
 case t of
   RBNode _ _ _ _ (RBNode Red _ _ _ _) -> rotateLeft t
   _ -> t

rotateRightIfNeeded : Dict k v -> Dict k v
rotateRightIfNeeded t =
 case t of
   RBNode _ _ _ (RBNode Red _ _ (RBNode Red _ _ _ _) _) _ -> rotateRight t
   _ -> t

otherColor c = case c of { Red -> Black ; Black -> Red }

color_flip : Dict k v -> Dict k v
color_flip t =
 case t of
   RBNode c1 bk bv (RBNode c2 ak av la ra) (RBNode c3 ck cv lc rc) ->
       RBNode (otherColor c1) bk bv
              (RBNode (otherColor c2) ak av la ra)
              (RBNode (otherColor c3) ck cv lc rc)
   _ -> Error.raise "color_flip called on a Empty or Node with a Empty child"

color_flipIfNeeded : Dict k v -> Dict k v
color_flipIfNeeded t =
 case t of
   RBNode _ _ _ (RBNode Red _ _ _ _) (RBNode Red _ _ _ _) -> color_flip t
   _ -> t

fixUp t = color_flipIfNeeded (rotateRightIfNeeded (rotateLeftIfNeeded t))

ensureBlackRoot : Dict k v -> Dict k v
ensureBlackRoot t =
  case t of
    RBNode Red k v l r -> RBNode Black k v l r
    _ -> t

-- Insert a key-value pair into a dictionary. Replaces value when there is
-- a collision.
insert : Comparable k -> v -> Dict (Comparable k) v -> Dict (Comparable k) v
insert k v t =  -- Invariant: t is a valid left-leaning rb tree
  let ins t =
      case t of
        RBEmpty -> RBNode Red k v RBEmpty RBEmpty
        RBNode c k' v' l r ->
          let h = case compare k k' of
                    LT -> RBNode c k' v' (ins l) r
                    EQ -> RBNode c k' v  l r  -- replace
                    GT -> RBNode c k' v' l (ins r)
          in  fixUp h
  in  ensureBlackRoot (ins t)
{--
      if not (invariants_hold t) then
          Error.raise "invariants broken before insert"
      else (let new_t = ensureBlackRoot (ins t) in
            if not (invariants_hold new_t) then
                Error.raise "invariants broken after insert"
            else new_t)
--}

-- Create a dictionary with one key-value pair.
singleton : Comparable k -> v -> Dict (Comparable k) v
singleton k v = insert k v RBEmpty

isRed : Dict k v -> Bool
isRed t =
  case t of
    RBNode Red _ _ _ _ -> True
    _ -> False

isRedLeft : Dict k v -> Bool
isRedLeft t =
  case t of
    RBNode _ _ _ (RBNode Red _ _ _ _) _ -> True
    _ -> False

isRedLeftLeft : Dict k v -> Bool
isRedLeftLeft t =
  case t of
    RBNode _ _ _ (RBNode _ _ _ (RBNode Red _ _ _ _) _) _ -> True
    _ -> False

isRedRight : Dict k v -> Bool
isRedRight t =
  case t of
    RBNode _ _ _ _ (RBNode Red _ _ _ _) -> True
    _ -> False

isRedRightLeft : Dict k v -> Bool
isRedRightLeft t =
  case t of
    RBNode _ _ _ _ (RBNode _ _ _ (RBNode Red _ _ _ _) _) -> True
    _ -> False

moveRedLeft : Dict k v -> Dict k v
moveRedLeft t =
  let t' = color_flip t in
  case t' of
    RBNode c k v l r ->
        case r of
          RBNode _ _ _ (RBNode Red _ _ _ _) _ ->
              color_flip (rotateLeft (RBNode c k v l (rotateRight r)))
          _ -> t'
    _ -> t'

moveRedRight : Dict k v -> Dict k v
moveRedRight t =
  let t' = color_flip t in
  if isRedLeftLeft t' then color_flip (rotateRight t') else t'

moveRedLeftIfNeeded : Dict k v -> Dict k v
moveRedLeftIfNeeded t =
  if isRedLeft t || isRedLeftLeft t then t else moveRedLeft t

moveRedRightIfNeeded : Dict k v -> Dict k v
moveRedRightIfNeeded t =
  if isRedRight t || isRedRightLeft t then t else moveRedRight t
  
deleteMin : Dict k v -> Dict k v
deleteMin t =
  let del t =
    case t of
      RBNode _ _ _ RBEmpty _ -> RBEmpty
      _ -> case moveRedLeftIfNeeded t of
             RBNode c k v l r -> fixUp (RBNode c k v (del l) r)
             RBEmpty -> RBEmpty
  in  ensureBlackRoot (del t)

{--
deleteMax t =
  let del t =
      let t' = if isRedLeft t then rotateRight t else t in
      case t' of
      { RBNode _ _ _ _ RBEmpty -> RBEmpty
      ; _ -> let t'' = moveRedRightIfNeeded t' in
             case t'' of
             { RBNode c k v l r -> fixUp (RBNode c k v l (del r))
             ; RBEmpty -> RBEmpty } }
  in  ensureBlackRoot (del t)
--}

-- Remove a key-value pair from a dictionary. If the key is not found,
-- no changes are made.
remove : Comparable k -> Dict (Comparable k) v -> Dict (Comparable k) v
remove k t =
  let eq_and_noRightNode t =
          case t of { RBNode _ k' _ _ RBEmpty -> k == k' ; _ -> False }
      eq t = case t of { RBNode _ k' _ _ _ -> k == k' ; _ -> False }
      delLT t = case moveRedLeftIfNeeded t of
                  RBNode c k' v l r -> fixUp (RBNode c k' v (del l) r)
                  RBEmpty -> Error.raise "delLT on Empty"
      delEQ t = case t of -- Replace with successor
                  RBNode c _ _ l r -> let (k',v') = min r in
                                      fixUp (RBNode c k' v' l (deleteMin r))
                  RBEmpty -> Error.raise "delEQ called on a Empty"
      delGT t = case t of
                  RBNode c k' v l r -> fixUp (RBNode c k' v l (del r))
                  RBEmpty -> Error.raise "delGT called on a Empty"
      del t = case t of
                RBEmpty -> RBEmpty
                RBNode _ k' _ _ _ ->
                    if k < k' then delLT t else
                        let u = if isRedLeft t then rotateRight t else t in
                        if eq_and_noRightNode u then RBEmpty else
                            let t' = moveRedRightIfNeeded t in
                            if eq t' then delEQ t' else delGT t'
  in  if member k t then ensureBlackRoot (del t) else t
{--
      if not (invariants_hold t) then
          Error.raise "invariants broken before remove"
      else (let t' = ensureBlackRoot (del t) in
            if invariants_hold t' then t' else
                Error.raise "invariants broken after remove")
--}

-- Apply a function to all values in a dictionary.
map : (a -> b) -> Dict (Comparable k) a -> Dict (Comparable k) b
map f t =
  case t of
    RBEmpty -> RBEmpty
    RBNode c k v l r -> RBNode c k (f v) (map f l) (map f r)

-- Fold over the key-value pairs in a dictionary, in order from lowest
-- key to highest key.
foldl : (Comparable k -> v -> b -> b) -> b -> Dict (Comparable k) v -> b
foldl f acc t =
  case t of
    RBEmpty -> acc
    RBNode _ k v l r -> foldl f (f k v (foldl f acc l)) r

-- Fold over the key-value pairs in a dictionary, in order from highest
-- key to lowest key.
foldr : (Comparable k -> v -> b -> b) -> b -> Dict (Comparable k) v -> b
foldr f acc t =
  case t of
    RBEmpty -> acc
    RBNode _ k v l r -> foldr f (f k v (foldr f acc r)) l

-- Combine two dictionaries. If there is a collision, preference is given
-- to the first dictionary.
union : Dict (Comparable k) v -> Dict (Comparable k) v -> Dict (Comparable k) v
union t1 t2 = foldl insert t2 t1

-- Keep a key-value pair when its key appears in the second dictionary.
-- Preference is given to values in the first dictionary.
intersect : Dict (Comparable k) v -> Dict (Comparable k) v -> Dict (Comparable k) v
intersect t1 t2 =
 let combine k v t = if k `member` t2 then insert k v t else t
 in  foldl combine empty t1

-- Keep a key-value pair when its key does not appear in the second dictionary.
-- Preference is given to the first dictionary.
diff : Dict (Comparable k) v -> Dict (Comparable k) v -> Dict (Comparable k) v
diff t1 t2 = foldl (\k v t -> remove k t) t1 t2

-- Get all of the keys in a dictionary.
keys : Dict (Comparable k) v -> [Comparable k]
keys t   = foldr (\k v acc -> k :: acc) [] t

-- Get all of the values in a dictionary.
values : Dict (Comparable k) v -> [v]
values t = foldr (\k v acc -> v :: acc) [] t

-- Convert a dictionary into an association list of key-value pairs.
toList : Dict (Comparable k) v -> [(Comparable k,v)]
toList t = foldr (\k v acc -> (k,v) :: acc) [] t

-- Convert an association list into a dictionary.
fromList : [(Comparable k,v)] -> Dict (Comparable k) v
fromList assocs = List.foldl (\(k,v) d -> insert k v d) empty assocs
