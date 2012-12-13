
module Dict (empty,singleton,insert
            ,lookup,findWithDefault
            ,remove,member
            ,foldl,foldr,map
            ,union,intersect,diff
            ,keys,values
            ,toList,fromList
            ) where

import Maybe (isJust)

data NColor = Red | Black

data RBTree k v = RBNode NColor k v (RBTree k v) (RBTree k v) | RBEmpty

empty = RBEmpty

raise err = throw (JavaScript.castStringToJSString err)

{-- Helpers for checking invariants

-- Check that the tree has an equal number of black nodes on each path
equal_pathLen t = 
  let path_numBlacks t =
     case t of
     { RBEmpty -> 1
     ; RBNode col _ _ l r ->
          let { bl = path_numBlacks l ; br = path_numBlacks r } in
          if bl /= br || bl == 0-1 || br == 0-1
              then 0-1
              else bl + (if col == Red then 0 else 1)
     }
  in 0-1 /= path_numBlacks t

rootBlack t = 
  case t of
  { RBEmpty -> True
  ; RBNode Black _ _ _ _ -> True
  ; _ -> False }

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


min t =
  case t of 
    RBNode _ k v RBEmpty _ -> (k,v)
    RBNode _ _ _ l _ -> min l
    RBEmpty -> raise "(min RBEmpty) is not defined"

{--
max t =
  case t of 
  { RBNode _ k v _ RBEmpty -> (k,v)
  ; RBNode _ _ _ _ r -> max r
  ; RBEmpty -> raise "(max RBEmpty) is not defined"
  }
--}

lookup k t =
 case t of
   RBEmpty -> Nothing
   RBNode _ k' v l r ->
    case compare k k' of
      LT -> lookup k l
      EQ -> Just v
      GT -> lookup k r

findWithDefault base k t =
 case t of
   RBEmpty -> base
   RBNode _ k' v l r ->
    case compare k k' of
      LT -> findWithDefault base k l
      EQ -> v
      GT -> findWithDefault base k r

{--
find k t =
 case t of
 { RBEmpty -> raise "Key was not found in dictionary!"
 ; RBNode _ k' v l r ->
    case compare k k' of
    { LT -> find k l
    ; EQ -> v
    ; GT -> find k r }
 }
--}

-- Does t contain k?
member k t = isJust $ lookup k t

rotateLeft t =
 case t of
   RBNode cy ky vy a (RBNode cz kz vz b c) -> RBNode cy kz vz (RBNode Red ky vy a b) c
   _ -> raise "rotateLeft of a node without enough children"

-- rotateRight -- the reverse, and 
-- makes Y have Z's color, and makes Z Red.
rotateRight t =
 case t of
   RBNode cz kz vz (RBNode cy ky vy a b) c -> RBNode cz ky vy a (RBNode Red kz vz b c)
   _ -> raise "rotateRight of a node without enough children"

rotateLeftIfNeeded t =
 case t of 
   RBNode _ _ _ _ (RBNode Red _ _ _ _) -> rotateLeft t
   _ -> t

rotateRightIfNeeded t =
 case t of 
   RBNode _ _ _ (RBNode Red _ _ (RBNode Red _ _ _ _) _) _ -> rotateRight t
   _ -> t

otherColor c = case c of { Red -> Black ; Black -> Red }

color_flip t =
 case t of
   RBNode c1 bk bv (RBNode c2 ak av la ra) (RBNode c3 ck cv lc rc) -> 
       RBNode (otherColor c1) bk bv
              (RBNode (otherColor c2) ak av la ra)
              (RBNode (otherColor c3) ck cv lc rc)
   _ -> raise "color_flip called on a RBEmpty or RBNode with a RBEmpty child"

color_flipIfNeeded t = 
 case t of
   RBNode _ _ _ (RBNode Red _ _ _ _) (RBNode Red _ _ _ _) -> color_flip t
   _ -> t

fixUp t = color_flipIfNeeded (rotateRightIfNeeded (rotateLeftIfNeeded t))


ensureBlackRoot t = 
  case t of
    RBNode Red k v l r -> RBNode Black k v l r
    _ -> t
     
-- Invariant: t is a valid left-leaning rb tree *)
insert k v t =
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
          raise "invariants broken before insert"
      else (let new_t = ensureBlackRoot (ins t) in
            if not (invariants_hold new_t) then
                raise "invariants broken after insert"
            else new_t)
--}

singleton k v = insert k v RBEmpty


isRed t =
  case t of
    RBNode Red _ _ _ _ -> True
    _ -> False

isRedLeft t =
  case t of
    RBNode _ _ _ (RBNode Red _ _ _ _) _ -> True
    _ -> False

isRedLeftLeft t =
  case t of
    RBNode _ _ _ (RBNode _ _ _ (RBNode Red _ _ _ _) _) _ -> True
    _ -> False

isRedRight t =
  case t of
    RBNode _ _ _ _ (RBNode Red _ _ _ _) -> True
    _ -> False

isRedRightLeft t =
  case t of
    RBNode _ _ _ _ (RBNode _ _ _ (RBNode Red _ _ _ _) _) -> True
    _ -> False


moveRedLeft t = 
  let t' = color_flip t in
  case t' of
    RBNode c k v l r ->
        case r of
          RBNode _ _ _ (RBNode Red _ _ _ _) _ ->
              color_flip (rotateLeft (RBNode c k v l (rotateRight r)))
          _ -> t'
    _ -> t'

moveRedRight t =
  let t' = color_flip t in
  if isRedLeftLeft t' then color_flip (rotateRight t') else t'

moveRedLeftIfNeeded t =
  if not (isRedLeft t) && not (isRedLeftLeft t) then moveRedLeft t else t

moveRedRightIfNeeded t =
  if not (isRedRight t) && not (isRedRightLeft t) then moveRedRight t else t
  
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

remove k t = 
  let eq_and_noRightNode t =
          case t of { RBNode _ k' _ _ RBEmpty -> k == k' ; _ -> False }
      eq t = case t of { RBNode _ k' _ _ _ -> k == k' ; _ -> False }
      delLT t = case moveRedLeftIfNeeded t of 
                  RBNode c k' v l r -> fixUp (RBNode c k' v (del l) r)
                  RBEmpty -> raise "delLT on RBEmpty"
      delEQ t = case t of -- Replace with successor
                  RBNode c _ _ l r -> let (k',v') = min r in
                                      fixUp (RBNode c k' v' l (deleteMin r))
                  RBEmpty -> raise "delEQ called on a RBEmpty"
      delGT t = case t of
                  RBNode c k' v l r -> fixUp (RBNode c k' v l (del r))
                  RBEmpty -> raise "delGT called on a RBEmpty"
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
          raise "invariants broken before remove"
      else (let t' = ensureBlackRoot (del t) in
            if invariants_hold t' then t' else
                raise "invariants broken after remove")
--}

map f t =
  case t of
    RBEmpty -> RBEmpty
    RBNode c k v l r -> RBNode c k (f v) (map f l) (map f r)

foldl f acc t =
  case t of
    RBEmpty -> acc
    RBNode _ k v l r -> foldl f (f k v (foldl f acc l)) r

foldr f acc t =
  case t of
    RBEmpty -> acc
    RBNode _ k v l r -> foldr f (f k v (foldr f acc r)) l

union t1 t2 = foldl insert t2 t1
intersect t1 t2 = foldl (\k v t -> if k `member` t2 then insert k v t else t) empty t1
diff t1 t2 = foldl (\k v t -> remove k t) t1 t2

keys t   = foldr (\k v acc -> k : acc) [] t
values t = foldr (\k v acc -> v : acc) [] t

toList t = foldr (\k v acc -> (k,v) : acc) [] t
fromList assocs = Elm.List.foldl (uncurry insert) empty assocs
