{-# LANGUAGE BangPatterns, MagicHash, TemplateHaskellQuotes, UnboxedTuples #-}
module Data.Map.Utils
  ( all
  , any
  , and
  , or
  , exists
  --
  , lookup_
  , require
  --
  , inserts
  --
  , toListWith
  , toListWithMaybe
  --
  , fromKeys
  , fromKeysA
  , fromValues
  --
  , fromSetA
  --
  , traverse_
  , traverseWithKey_
  , mtraverseWithKey
  , mindexedTraverse
  , mindexedTraverseWithKey
  , traverse#
  , traverseWithKey#
  , indexedTraverse#
  , indexedTraverseWithKey#
  --
  , mmap
  , mmapWithKey
  , mapKeysMonotonicWith
  --
  , foldrM
  , foldrWithKeyM
  , foldlWithKeyM
  , foldlWithKey#
  , ifoldlWithKey#
  --
  , intersectionSize
  , intersectSetWithKeyA
  , mintersectWithKey
  , uniqueKeys
  , invert
  , invertWith
  )
  where


import Prelude hiding (all, any, and, or)
import Control.Applicative (liftA3)
import Data.Map.Internal (Map(..), balance, link, link2, splitLookup)
import qualified Data.Map.Strict as Map
import qualified Data.Set.Internal as Set
import GHC.Exts (Int(..))
import GHC.Prim
import Language.Haskell.TH.Syntax (Q, Exp)
import qualified Language.Haskell.TH.Syntax as TH

import qualified Crash



-- BOOL HELPERS


{-# INLINE all #-}
all :: (a -> Bool) -> Map k a -> Bool
all isGood =
    go
  where
    go Tip             = True
    go (Bin _ _ v l r) = isGood v && go l && go r


{-# INLINE any #-}
any :: (a -> Bool) -> Map k a -> Bool
any isGood =
    go
  where
    go Tip             = False
    go (Bin _ _ v l r) = isGood v || go l || go r


and :: Map k Bool -> Bool
and dict =
  case dict of
    Tip           -> True
    Bin _ _ b l r -> b && and l && and r


or :: Map k Bool -> Bool
or dict =
  case dict of
    Tip           -> False
    Bin _ _ b l r -> b || or l || or r


{-# INLINABLE exists #-}
exists :: (Ord k, Eq a) => k -> a -> Map k a -> Bool
exists =
    go
  where
    go !key value dict =
      case dict of
        Tip           -> False
        Bin _ k v l r ->
          case compare key k of
            LT -> go key value l
            GT -> go key value r
            EQ -> value == v



-- LOOKUP_


{-# INLINABLE lookup_ #-}
lookup_ :: (Ord k) => k -> Map.Map k a -> r -> (a -> r) -> r
lookup_ key dict err ok =
    go dict
  where
    go d =
      case d of
        Tip ->
          err

        Bin _ k v l r ->
          case compare key k of
            LT -> go l
            GT -> go r
            EQ -> ok v



-- REQUIRE


require :: TH.Name -> Q Exp
require =
  Crash.crashable 'require_


{-# INLINABLE require_ #-}
require_ :: (Ord k) => Crash.Module -> Crash.Name -> Crash.Line -> k -> Map.Map k a -> (k -> [Char]) -> a
require_ modul name line key dict keyToChars =
    go dict
  where
    go d =
      case d of
        Tip ->
          Crash.crash_ modul name line $
            "unable to find \"" ++ keyToChars key ++ "\""

        Bin _ k v l r ->
          case compare key k of
            LT -> go l
            GT -> go r
            EQ -> v



-- INSERTS


inserts :: Ord k => (a -> v) -> (a -> v -> v) -> k -> a -> Map k v -> Map k v
inserts one cons key value =
    go
  where
    go dict =
      case dict of
        Tip ->
          Bin 1 key (one value) Tip Tip

        Bin size k v l r ->
          case compare key k of
            LT -> balance k v (go l) r
            GT -> balance k v l (go r)
            EQ -> let !v' = cons value v in Bin size key v' l r



-- TO LIST WITH


toListWith :: (k -> v -> a) -> Map.Map k v -> [a]
toListWith func dict =
  Map.foldrWithKey (\k v xs -> func k v : xs) [] dict


toListWithMaybe :: (k -> v -> Maybe a) -> Map.Map k v -> [a]
toListWithMaybe func dict =
    Map.foldrWithKey cons [] dict
  where
    cons k v xs =
      case func k v of
        Just x  -> x:xs
        Nothing -> xs



-- FROM KEYS


fromKeys :: (Ord k) => (k -> v) -> [k] -> Map.Map k v
fromKeys toValue keys =
  Map.fromList $ map (\k -> (k, toValue k)) keys


fromKeysA :: (Applicative f, Ord k) => (k -> f v) -> [k] -> f (Map.Map k v)
fromKeysA toValue keys =
  Map.fromList <$> traverse (\k -> (,) k <$> toValue k) keys


fromValues :: (Ord k) => (v -> k) -> [v] -> Map.Map k v
fromValues toKey values =
  Map.fromList $ map (\v -> (toKey v, v)) values



-- FROM SET


fromSetA :: (Applicative f) => (k -> f a) -> Set.Set k -> f (Map k a)
fromSetA func =
    go
  where
    go set =
      case set of
        Set.Tip         -> pure Tip
        Set.Bin s k l r -> liftA3 (\l1 v r1 -> Bin s k v l1 r1) (go l) (func k) (go r)



-- TRAVERSE


{-# INLINE traverse_ #-}
traverse_ :: Applicative f => (a -> f ()) -> Map k a -> f ()
traverse_ f =
    go
  where
    go Tip             = pure ()
    go (Bin 1 _ v _ _) = f v
    go (Bin _ _ v l r) = go l *> f v *> go r


{-# INLINE traverseWithKey_ #-}
traverseWithKey_ :: (Applicative f) => (k -> a -> f ()) -> Map k a -> f ()
traverseWithKey_ f =
    go
  where
    go Tip             = pure ()
    go (Bin 1 k v _ _) = f k v
    go (Bin _ k v l r) = go l *> f k v *> go r


mtraverseWithKey :: (Applicative f, Monoid m) => (k -> a -> f m) -> Map k a -> f m
mtraverseWithKey func =
    go
  where
    go dict =
      case dict of
        Tip           -> pure mempty
        Bin 1 k v _ _ -> func k v
        Bin _ k v l r -> liftA2 mappend (go l) (liftA2 mappend (func k v) (go r))


mindexedTraverse :: (Applicative f, Monoid m) => (Int -> a -> f m) -> Map k a -> f m
mindexedTraverse func =
    go 0
  where
    go i dict =
      case dict of
        Tip           -> pure mempty
        Bin 1 _ v _ _ -> func i v
        Bin _ _ v l r ->
          let
            !j = i + Map.size l
          in
          liftA2 mappend (go i l) (liftA2 mappend (func j v) (go (j + 1) r))


mindexedTraverseWithKey :: (Applicative f, Monoid m) => (Int -> k -> a -> f m) -> Map k a -> f m
mindexedTraverseWithKey func =
    go 0
  where
    go i dict =
      case dict of
        Tip           -> pure mempty
        Bin 1 k v _ _ -> func i k v
        Bin _ k v l r ->
          let
            !j = i + Map.size l
          in
          liftA2 mappend (go i l) (liftA2 mappend (func j k v) (go (j + 1) r))


traverse# :: (a -> State# s -> State# s) -> Map k a -> State# s -> State# s
traverse# func dict s0 =
  case dict of
    Tip           -> s0
    Bin 1 _ v _ _ -> func v s0
    Bin _ _ v l r ->
      case traverse# func l s0 of { s1 ->
      case func v           s1 of { s2 ->
      case traverse# func r s2 of { s3 -> s3 }}}


traverseWithKey# :: (k -> a -> State# s -> State# s) -> Map k a -> State# s -> State# s
traverseWithKey# func dict s0 =
  case dict of
    Tip           -> s0
    Bin 1 k v _ _ -> func k v s0
    Bin _ k v l r ->
      case traverseWithKey# func l s0 of { s1 ->
      case func k v                s1 of { s2 ->
      case traverseWithKey# func r s2 of { s3 -> s3 }}}


indexedTraverse# :: (Int# -> a -> State# s -> State# s) -> Map k a -> State# s -> State# s
indexedTraverse# func =
    go 0#
  where
    go i dict s0 =
      case dict of
        Tip           -> s0
        Bin 1 _ v _ _ -> func i v s0
        Bin _ _ v l r ->
          let
            !(I# s) = Map.size l
            !j      = i +# s
          in
          case go   i        l s0 of { s1 ->
          case func j        v s1 of { s2 ->
          case go  (j +# 1#) r s2 of { s3 -> s3 }}}


indexedTraverseWithKey# :: (Int# -> k -> a -> State# s -> State# s) -> Map k a -> State# s -> State# s
indexedTraverseWithKey# func =
    go 0#
  where
    go i dict s0 =
      case dict of
        Tip           -> s0
        Bin 1 k v _ _ -> func i k v s0
        Bin _ k v l r ->
          let
            !(I# s) = Map.size l
            !j      = i +# s
          in
          case go   i        l s0 of { s1 ->
          case func j      k v s1 of { s2 ->
          case go  (j +# 1#) r s2 of { s3 -> s3 }}}



-- MMAP


{-# INLINE mmap #-}
mmap :: Monoid m => (a -> m) -> Map k a -> m
mmap func =
    go
  where
    go Tip             = mempty
    go (Bin 1 _ v _ _) = func v
    go (Bin _ _ v l r) = go l `mappend` (func v `mappend` go r)



{-# INLINE mmapWithKey #-}
mmapWithKey :: Monoid m => (k -> a -> m) -> Map k a -> m
mmapWithKey func =
    go
  where
    go Tip             = mempty
    go (Bin 1 k v _ _) = func k v
    go (Bin _ k v l r) = go l `mappend` (func k v `mappend` go r)


mapKeysMonotonicWith :: (k -> a -> k') -> Map k a -> Map k' a
mapKeysMonotonicWith func =
    go
  where
    go dict =
      case dict of
        Tip           -> Tip
        Bin s k v l r -> Bin s (func k v) v (go l) (go r)



-- FOLDR M


{-# INLINE foldrM #-}
foldrM :: (Monad m) => (a -> s -> m s) -> s -> Map k a -> m s
foldrM step base =
    go base
  where
    go state dict =
      case dict of
        Tip           -> return state
        Bin _ _ v l r -> go state r >>= step v >>= (\s -> go s l)


{-# INLINE foldrWithKeyM #-}
foldrWithKeyM :: (Monad m) => (k -> a -> s -> m s) -> s -> Map k a -> m s
foldrWithKeyM step base =
    go base
  where
    go state dict =
      case dict of
        Tip           -> return state
        Bin _ k v l r -> go state r >>= step k v >>= (\s -> go s l)


{-# INLINE foldlWithKeyM #-}
foldlWithKeyM :: (Monad m) => (k -> a -> s -> m s) -> s -> Map k a -> m s
foldlWithKeyM step base =
    go base
  where
    go state dict =
      case dict of
        Tip           -> return state
        Bin _ k v l r -> go state l >>= step k v >>= (\s -> go s r)


foldlWithKey# :: (k -> a -> state -> State# s -> (# State# s, state #)) -> state -> Map k a -> State# s -> (# State# s, state #)
foldlWithKey# func =
    go
  where
    go state0 dict s0 =
      case dict of
        Tip           -> (# s0, state0 #)
        Bin 1 k v _ _ -> func k v state0 s0
        Bin _ k v l r ->
          case go       state0 l s0 of { (# s1, state1 #) ->
          case func k v state1   s1 of { (# s2, state2 #) -> go state2 r s2 }}


ifoldlWithKey# :: (k -> a -> Int# -> State# s -> (# State# s, Int# #)) -> Int# -> Map k a -> State# s -> (# State# s, Int# #)
ifoldlWithKey# func =
    go
  where
    go state0 dict s0 =
      case dict of
        Tip           -> (# s0, state0 #)
        Bin 1 k v _ _ -> func k v state0 s0
        Bin _ k v l r ->
          case go       state0 l s0 of { (# s1, state1 #) ->
          case func k v state1   s1 of { (# s2, state2 #) -> go state2 r s2 }}



-- INTERSECTION SIZE


{-# INLINABLE intersectionSize #-}
intersectionSize :: (Ord k) => Map k a -> Map k b -> Int
intersectionSize Tip _ = 0
intersectionSize _ Tip = 0
intersectionSize (Bin _ k _ l r) t2 =
  case m of
    Just _  -> s1 + s2 + 1
    Nothing -> s1 + s2
  where
    !(l2, m, r2) = splitLookup k t2
    !s1 = intersectionSize l l2
    !s2 = intersectionSize r r2



-- INTERSECT SET WITH A


{-# INLINABLE intersectSetWithKeyA #-}
intersectSetWithKeyA :: (Ord k, Applicative f) => (k -> a -> f r) -> Map k a -> Set.Set k -> f (Map k r)
intersectSetWithKeyA func =
    go
  where
    go dict set =
      case dict of
        Tip ->
          pure Tip

        Bin _ k v l r ->
          case set of
            Set.Tip ->
              pure Tip

            Set.Bin _ _ _ _ ->
              case Set.splitMember k set of
                (left,found,right) ->
                  if found
                  then liftA3 (\l' v' r' -> link k v' l' r') (go l left) (func k v) (go r right)
                  else liftA2  link2                         (go l left)            (go r right)



-- M INTERSECT WITH KEY


{-# INLINABLE mintersectWithKey #-}
mintersectWithKey :: (Ord k, Monoid m) => (k -> a -> b -> m) -> Map k a -> Map k b -> m
mintersectWithKey _ Tip _ = mempty
mintersectWithKey _ _ Tip = mempty
mintersectWithKey f (Bin _ k v l r) t2 =
  case splitLookup k t2 of
    !(l2, m, r2) ->
      case m of
        Just v2 -> mintersectWithKey f l l2 `mappend` (f k v v2 `mappend` mintersectWithKey f r r2)
        Nothing -> mintersectWithKey f l l2 `mappend` mintersectWithKey f r r2



-- UNIQUE KEYS


{-# INLINABLE uniqueKeys #-}
uniqueKeys :: (Ord k) => Set.Set k -> Map k v -> Set.Set k
uniqueKeys set dict =
  case set of
    Set.Tip ->
      Set.Tip

    Set.Bin size _ _ _ ->
      case dict of
        Tip ->
          set

        Bin _ key _ left right ->
          case Set.split key set of
            (l1,r1)
              | Set.size l2 + Set.size r2 == size -> set
              | otherwise                         -> Set.merge l2 r2
              where
                !l2 = uniqueKeys l1 left
                !r2 = uniqueKeys r1 right



-- INVERT


invert :: (Ord b) => Map.Map a b -> Map.Map b a
invert dict =
  Map.fromList $ toListWith (\a b -> (b,a)) dict


{-# INLINE invertWith #-}
invertWith :: (Ord b') => (a -> a') -> (b -> b') -> Map.Map a b -> Map.Map b' a'
invertWith goA goB dict =
  Map.fromList $ toListWith (\a b -> (goB b, goA a)) dict


