
module Map (empty,singleton,insert,lookup,remove,member,fold) where

import Data.Maybe (isJust)

data NColor = Red | Black

data RBTree k v = Node NColor k v (RBTree k v) (RBTree k v) | Empty

raise = console.log

empty = Empty

-- Helpers for checking invariants

-- Check that the tree has an equal number of black nodes on each path
equal_pathLen t = 
  let path_numBlacks t =
     case t of
     { Empty -> 1
     ; Node col _ _ l r ->
          let { bl = path_numBlacks l ; br = path_numBlacks r } in
          if bl /= br || bl == 0-1 || br == 0-1
              then 0-1
              else bl + (if col == Red then 0 else 1)
     }
  in 0-1 /= path_numBlacks t

rootBlack t = 
  case t of
  { Empty -> True
  ; Node Black _ _ _ _ -> True
  ; _ -> False }

redBlack_children t = 
  case t of 
  { Node Red _ _ (Node Red _ _ _ _) _ -> False
  ; Node Red _ _ _ (Node Red _ _ _ _) -> False
  ; Empty -> True
  ; Node _ _ _ l r -> redBlack_children l && redBlack_children r
  }

findExtreme f t =
  case t of
  { Empty -> Nothing
  ; Node c k _ l r ->
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
  { Empty -> True
  ; Node c k v l r ->
      let (lmax,rmin) = (findmaxRbt l, findminRbt r) in
      olte lmax (Just k) && olte (Just k) rmin && ordered l && ordered r
  }

-- Check that there aren't any right red nodes in the tree *)
leftLeaning t = 
  case t of
  { Empty -> True
  ; Node _ _ _ (Node Black _ _ _ _) (Node Red _ _ _ _) -> False
  ; Node _ _ _ Empty (Node Red _ _ _ _) -> False
  ; Node _ _ _ l r -> (leftLeaning l) && (leftLeaning r)
  }

invariants_hold t =
  ordered t && rootBlack t && redBlack_children t && 
  equal_pathLen t && leftLeaning t

--** End invariant helpers *****


min t =
  case t of 
  { Node _ k v Empty _ -> (k,v)
  ; Node _ _ _ l _ -> min l
  ; Empty -> console.log "(min Empty) is not defined"
  }

max t =
  case t of 
  { Node _ k v _ Empty -> (k,v)
  ; Node _ _ _ _ r -> max r
  ; Empty -> console.log "(max Empty) is not defined"
  }

lookup k t =
 case t of
 { Empty -> Nothing
 ; Node _ k' v l r ->
    case compare k k' of
    { LT -> lookup k l
    ; EQ -> Just v
    ; GT -> lookup k r }
 }

-- Does t contain k?
member k t = isJust $ lookup k t

rotateLeft t =
 case t of
 { Node cy ky vy a (Node cz kz vz b c) -> Node cy kz vz (Node Red ky vy a b) c
 ; _ -> raise "rotateLeft of a node without enough children" }

-- rotateRight -- the reverse, and 
-- makes Y have Z's color, and makes Z Red.
rotateRight t =
 case t of
 { Node cz kz vz (Node cy ky vy a b) c -> Node cz ky vy a (Node Red kz vz b c)
 ; _ -> raise "rotateRight of a node without enough children" }

rotateLeftIfNeeded t =
 case t of 
 { Node _ _ _ _ (Node Red _ _ _ _) -> rotateLeft t
 ; _ -> t }

rotateRightIfNeeded t =
 case t of 
 { Node _ _ _ (Node Red _ _ (Node Red _ _ _ _) _) _ -> rotateRight t
 ; _ -> t }

otherColor c = case c of { Red -> Black ; Black -> Red }

color_flip t =
 case t of
 { Node c1 bk bv (Node c2 ak av la ra) (Node c3 ck cv lc rc) -> 
   Node (otherColor c1) bk bv
            (Node (otherColor c2) ak av la ra)
            (Node (otherColor c3) ck cv lc rc)
 ; _ -> raise "color_flip called on a Empty or Node with a Empty child" }

color_flipIfNeeded t = 
 case t of
 { Node _ _ _ (Node Red _ _ _ _) (Node Red _ _ _ _) -> color_flip t
 ; _ -> t }

fixUp t = color_flipIfNeeded (rotateRightIfNeeded (rotateLeftIfNeeded t))


ensureBlackRoot t = 
  case t of
  { Node Red k v l r -> Node Black k v l r
  ; _ -> t }
     
-- Invariant: t is a valid left-leaning rb tree *)
insert k v t =
  let ins t =
      case t of
      { Empty -> Node Red k v Empty Empty
      ; Node c k' v' l r ->
          let h = case compare k k' of
                  { LT -> Node c k' v' (ins l) r
                  ; EQ -> Node c k' v  l r  -- replace
                  ; GT -> Node c k' v' l (ins r) }
          in  fixUp h }
  in  if not (invariants_hold t) then
          raise "invariants broken before insert"
      else (let new_t = ensureBlackRoot (ins t) in
            if not (invariants_hold new_t) then
                raise "invariants broken after insert"
            else new_t)

singleton k v = insert k v Empty


isRed t =
  case t of
  { Node Red _ _ _ _ -> True
  ; _ -> False }

isRedLeft t =
  case t of
  { Node _ _ _ (Node Red _ _ _ _) _ -> True
  ; _ -> False }

isRedLeftLeft t =
  case t of
  { Node _ _ _ (Node _ _ _ (Node Red _ _ _ _) _) _ -> True
  ; _ -> False }

isRedRight t =
  case t of
  { Node _ _ _ _ (Node Red _ _ _ _) -> True
  ; _ -> False }

isRedRightLeft t =
  case t of
  { Node _ _ _ _ (Node _ _ _ (Node Red _ _ _ _) _) -> True
  ; _ -> False }


moveRedLeft t = 
  let t' = color_flip t in
  case t' of
  { Node c k v l r ->
        case r of
        { Node _ _ _ (Node Red _ _ _ _) _ ->
              color_flip (rotateLeft (Node c k v l (rotateRight r)))
        ; _ -> t' }
  ; _ -> t' }

moveRedRight t =
  let t' = color_flip t in
  if isRedLeftLeft t' then color_flip (rotateRight t') else t'

moveRedLeftIfNeeded t =
  if not (isRedLeft t) && not (isRedLeftLeft t)
  then moveRedLeft t
  else t

moveRedRightIfNeeded t =
  if not (isRedRight t) && not (isRedRightLeft t)
  then moveRedRight t
  else t
  
deleteMin t = 
  let del t =
    case t of 
    { Node _ _ _ Empty _ -> Empty
    ; _ -> let t' = moveRedLeftIfNeeded t in
           case t' of
           { Node c k v l r -> fixUp (Node c k v (del l) r)
           ; Empty -> Empty }
    }
  in  ensureBlackRoot (del t)

deleteMax t =
  let del t =
      let t' = if isRedLeft t then rotateRight t else t in
      case t' of
      { Node _ _ _ _ Empty -> Empty
      ; _ -> let t'' = moveRedRightIfNeeded t' in
             case t'' of
             { Node c k v l r -> fixUp (Node c k v l (del r))
             ; Empty -> Empty } }
  in  ensureBlackRoot (del t)

remove k t = 
  let {
    eq_and_noRightNode t = case t of { Node _ k' _ _ Empty -> k == k' ; _ -> False }
  ; eq t = case t of { Node _ k' _ _ _ -> k == k' ; _ -> False }
  ; delLT t = 
    let t' = moveRedLeftIfNeeded t in
    case t' of 
    { Node c k' v l r -> fixUp (Node c k' v (del l) r)
    ; Empty -> raise "delLT on Empty" }
  ; delEQ t =
    case t of -- Replace with successor
    { Node c _ _ l r ->
          let (k',v') = min r in
          fixUp (Node c k' v' l (deleteMin r))
    ; Empty -> raise "delEQ called on a Empty" }
  ; delGT t =
    case t of
    { Node c k' v l r -> fixUp (Node c k' v l (del r))
    ; Empty -> raise "delGT called on a Empty" }
  ; del t =
    case t of 
    { Empty -> Empty
    ; Node _ k' _ _ _ -> 
      if k < k' then delLT t
      else (let t' = if isRedLeft t then rotateRight t else t in
            if eq_and_noRightNode t' then Empty
            else (let t = moveRedRightIfNeeded t in
                  if eq t then delEQ t else delGT t)) }
  }
  in  if not (invariants_hold t) then
          raise "invariants broken before remove"
      else (let t' = ensureBlackRoot (del t) in
            if invariants_hold t' then t' else
                raise "invariants broken after remove")

fold f acc t =
  case t of
  { Empty -> acc
  ; Node _ k v l r -> fold f (f k v (fold f acc l)) r
  }

{--
--}