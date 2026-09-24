{-# LANGUAGE MagicHash, TemplateHaskell #-}
module Data.NonEmptyMap
  ( Map
  , singleton
  , size
  , lookup
  , insertWith
  , findWithDefault
  --
  , map
  , mapWithKey
  , mapWithIndex
  , mmap
  , mmapWithKey
  , sequenceA
  , traverse
  , traverseWithKey
  , traverse_
  , traverseWithKey_
  , mtraverseWithKey
  , mindexedTraverse
  , mindexedTraverseWithKey
  , indexedTraverse#
  , indexedTraverseWithKey#
  --
  , foldr
  , foldrWithKey
  , foldrM
  , foldrWithKeyM
  --
  , all
  , any
  , and
  , or
  --
  , Trimmed(..)
  , trim
  --
  , union
  , unionWith
  , intersectWith
  , intersectWithKey
  , intersectionSize
  , withoutMap
  , diff
  , diffWithKey
  , unsafeDiff
  , unsafeOverlapWith
  , unsafeOverlapWithA
  , unsafeOverlapWithKeyA
  --
  , toDict
  , toList
  , toList1
  , toListNE
  , toListWithNE
  , toValues
  , toValues1
  , toValuesNE
  --
  , getMaxKey
  , maxView
  , fromDict
  , fromList
  , fromDistinctAscList
  --
  , encode8, decode8
  , encode16, decode16
  , encode32, decode32
  )
  where


import Prelude hiding (any, all, and, or, foldr, map, lookup, sequenceA, traverse)
import Control.Applicative (liftA3)
import Data.Functor.Classes (Eq1, liftEq)
import qualified Data.Map as Map
import qualified Data.Map.Utils as Map
import qualified Data.Map.Internal as Map
import qualified Data.Traversable as T
import GHC.Prim (State#, Int#)
import Language.Haskell.TH.Syntax (Q, Exp)
import qualified Language.Haskell.TH.Syntax as TH

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified Crash



-- MAP


newtype Map k a =
  Map (Map.Map k a)
  deriving (Eq, Ord)


instance (Eq k) => Eq1 (Map k) where
  liftEq eq (Map x) (Map y) = liftEq eq x y


singleton :: k -> a -> Map k a
singleton k a =
  Map (Map.singleton k a)


size :: Map k a -> Int
size (Map dict) =
  Map.size dict


lookup :: (Ord k) => k -> Map k a -> Maybe a
lookup key (Map dict) =
  Map.lookup key dict


insertWith :: (Ord k) => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith func k v (Map dict) =
  Map (Map.insertWith func k v dict)


findWithDefault :: (Ord k) => a -> k -> Map k a -> a
findWithDefault backup key (Map dict) =
  Map.findWithDefault backup key dict



-- MAP


map :: (a -> b) -> Map k a -> Map k b
map func (Map dict) =
  Map (Map.map func dict)


mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey func (Map dict) =
  Map (Map.mapWithKey func dict)


mapWithIndex :: (Word -> a -> b) -> Map k a -> Map k b
mapWithIndex func (Map dict) =
  Map $ snd $ Map.mapAccum (\s a -> (s + 1, func s a)) 0 dict


mmap :: Monoid m => (a -> m) -> Map k a -> m
mmap func (Map dict) =
  Map.mmap func dict


mmapWithKey :: Monoid m => (k -> a -> m) -> Map k a -> m
mmapWithKey func (Map dict) =
  Map.foldMapWithKey func dict


sequenceA :: (Applicative f) => Map k (f a) -> f (Map k a)
sequenceA (Map dict) =
  Map <$> T.sequenceA dict


traverse :: (Applicative f) => (a -> f b) -> Map k a -> f (Map k b)
traverse func (Map dict) =
  Map <$> T.traverse func dict


traverseWithKey :: (Applicative f) => (k -> a -> f b) -> Map k a -> f (Map k b)
traverseWithKey func (Map dict) =
  Map <$> Map.traverseWithKey func dict


traverse_ :: (Applicative f) => (a -> f ()) -> Map k a -> f ()
traverse_ func (Map dict) =
  Map.traverse_ func dict


traverseWithKey_ :: (Applicative f) => (k -> a -> f ()) -> Map k a -> f ()
traverseWithKey_ func (Map dict) =
  Map.traverseWithKey_ func dict


mtraverseWithKey :: (Applicative f, Monoid m) => (k -> a -> f m) -> Map k a -> f m
mtraverseWithKey func (Map dict) =
  Map.mtraverseWithKey func dict


mindexedTraverse :: (Applicative f, Monoid m) => (Int -> a -> f m) -> Map k a -> f m
mindexedTraverse func (Map dict) =
  Map.mindexedTraverse func dict


mindexedTraverseWithKey :: (Applicative f, Monoid m) => (Int -> k -> a -> f m) -> Map k a -> f m
mindexedTraverseWithKey func (Map dict) =
  Map.mindexedTraverseWithKey func dict


indexedTraverse# :: (Int# -> a -> State# s -> State# s) -> Map k a -> State# s -> State# s
indexedTraverse# func (Map dict) s =
  Map.indexedTraverse# func dict s


indexedTraverseWithKey# :: (Int# -> k -> a -> State# s -> State# s) -> Map k a -> State# s -> State# s
indexedTraverseWithKey# func (Map dict) s =
  Map.indexedTraverseWithKey# func dict s



-- FOLDR


foldr :: (a -> b -> b) -> b -> Map k a -> b
foldr step base (Map dict) =
  Map.foldr step base dict


foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey step base (Map dict) =
  Map.foldrWithKey step base dict


foldrM :: (Monad m) => (a -> b -> m b) -> b -> Map k a -> m b
foldrM step base (Map dict) =
  Map.foldrM step base dict


foldrWithKeyM :: (Monad m) => (k -> a -> b -> m b) -> b -> Map k a -> m b
foldrWithKeyM step base (Map dict) =
  Map.foldrWithKeyM step base dict



-- BOOL HELPERS


all :: (a -> Bool) -> Map k a -> Bool
all isGood (Map dict) =
  Map.all isGood dict


any :: (a -> Bool) -> Map k a -> Bool
any isGood (Map dict) =
  Map.any isGood dict


and :: Map k Bool -> Bool
and (Map dict) =
  Map.and dict


or :: Map k Bool -> Bool
or (Map dict) =
  Map.or dict



-- TRIM


data Trimmed k v
  = None
  | One k v
  | More (Map k v)


trim :: (Eq v) => v -> Map k v -> Trimmed k v
trim target (Map dict) =
  case Map.filter (== target) dict of
    Map.Tip               -> None
    Map.Bin 1 k v _ _     -> One k v
    d@(Map.Bin _ _ _ _ _) -> More (Map d)



-- UNION/INTERSECT


union :: (Ord k) => Map k a -> Map k a -> Map k a
union (Map dict1) (Map dict2) =
  Map (Map.union dict1 dict2)


unionWith :: (Ord k) => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith func (Map dict1) (Map dict2) =
  Map (Map.unionWith func dict1 dict2)


intersectWith :: (Ord k) => (a -> b -> c) -> Map k a -> Map k b -> Map.Map k c
intersectWith func (Map dict1) (Map dict2) =
  Map.intersectionWith func dict1 dict2


intersectWithKey :: (Ord k) => (k -> a -> b -> c) -> Map k a -> Map k b -> Map.Map k c
intersectWithKey func (Map dict1) (Map dict2) =
  Map.intersectionWithKey func dict1 dict2


intersectionSize :: (Ord k) => Map k a -> Map k b -> Int
intersectionSize (Map dict1) (Map dict2) =
  Map.intersectionSize dict1 dict2


withoutMap :: (Ord k) => Map k a -> Map.Map k b -> Map.Map k a
withoutMap (Map dict1) dict2 =
  Map.difference dict1 dict2


diff :: (Ord k) => Map k a -> Map k b -> Map.Map k a
diff (Map dict1) (Map dict2) =
  Map.difference dict1 dict2


diffWithKey :: (Ord k) => (k -> a -> v) -> (k -> a -> b -> v) -> Map k a -> Map.Map k b -> Map k v
diffWithKey left both (Map dict1) dict2 =
  Map $ Map.merge (Map.mapMissing left) Map.dropMissing (Map.zipWithMatched both) dict1 dict2


unsafeDiff :: (Ord k) => Map k a -> Map k b -> Map k a
unsafeDiff (Map dict1) (Map dict2) =
  Map (Map.difference dict1 dict2)


unsafeOverlapWith     :: TH.Name -> Q Exp
unsafeOverlapWithA    :: TH.Name -> Q Exp
unsafeOverlapWithKeyA :: TH.Name -> Q Exp

unsafeOverlapWith     = Crash.crashable 'unsafeOverlapWith_
unsafeOverlapWithA    = Crash.crashable 'unsafeOverlapWithA_
unsafeOverlapWithKeyA = Crash.crashable 'unsafeOverlapWithKeyA_


unsafeOverlapWith_ :: (Ord k) => Crash.Module -> Crash.Name -> Crash.Line -> (a -> b -> c) -> Map k a -> Map k b -> Map k c
unsafeOverlapWith_ modul name line func (Map dict1) (Map dict2) =
    Map (loop dict1 dict2)
  where
    loop Map.Tip Map.Tip = Map.Tip
    loop Map.Tip _       = Crash.crash_ modul name line "unsafeOverlapWith"
    loop (Map.Bin n k v l r) t2 =
      case Map.splitLookup k t2 of
        (l2, mv2, r2) ->
          case mv2 of
            Nothing -> Crash.crash_ modul name line "unsafeOverlapWith"
            Just v2 -> Map.Bin n k (func v v2) (loop l l2) (loop r r2)


unsafeOverlapWithA_ :: (Applicative f, Ord k) => Crash.Module -> Crash.Name -> Crash.Line -> (a -> b -> f c) -> Map k a -> Map k b -> f (Map k c)
unsafeOverlapWithA_ modul name line func =
  unsafeOverlapWithKeyA_ modul name line (\_ a b -> func a b)


unsafeOverlapWithKeyA_ :: (Applicative f, Ord k) => Crash.Module -> Crash.Name -> Crash.Line -> (k -> a -> b -> f c) -> Map k a -> Map k b -> f (Map k c)
unsafeOverlapWithKeyA_ modul name line func (Map dict1) (Map dict2) =
    Map <$> loop dict1 dict2
  where
    loop Map.Tip Map.Tip = pure Map.Tip
    loop Map.Tip _       = Crash.crash_ modul name line "unsafeOverlapWithKeyA"
    loop (Map.Bin n k v l r) t2 =
      case Map.splitLookup k t2 of
        (l2, mv2, r2) ->
          case mv2 of
            Nothing -> Crash.crash_ modul name line "unsafeOverlapWithKeyA"
            Just v2 -> liftA3 (Map.Bin n k) (func k v v2) (loop l l2) (loop r r2)



-- FROM/TO


toDict :: (Ord k) => Map k a -> Map.Map k a
toDict (Map dict) =
  dict


toList :: Map k a -> [(k,a)]
toList (Map dict) =
  Map.toList dict


toList1 :: (Ord k) => Map k a -> ([(k,a)], (k,a))
toList1 (Map dict) =
  case dict of
    Map.Bin 1 k v _ _ -> ([], (k,v))
    Map.Bin _ k v l r -> case maxViewHelp k v l r of { Max d km vm -> (Map.toList d, (km,vm)) }
    Map.Tip           -> $(Crash.crash 'toList1) "NonEmptyMap cannot be empty"


toListNE :: Map k a -> ((k,a), [(k,a)])
toListNE (Map dict) =
  case Map.toList dict of
    p:ps -> (p,ps)
    []   -> $(Crash.crash 'toListNE) "NonEmptyMap cannot be empty"


toListWithNE :: (k -> a -> v) -> Map k a -> (v, [v])
toListWithNE func (Map dict) =
  case Map.toListWith func dict of
    v:vs -> (v,vs)
    []   -> $(Crash.crash 'toListWithNE) "NonEmptyMap cannot be empty"


toValues :: Map k a -> [a]
toValues (Map dict) =
  Map.elems dict


toValues1 :: (Ord k) => Map k a -> ([a], a)
toValues1 (Map dict) =
  case dict of
    Map.Bin 1 _ v _ _ -> ([], v)
    Map.Bin _ k v l r -> case maxViewHelp k v l r of { Max d _ vm -> (Map.elems d, vm) }
    Map.Tip           -> $(Crash.crash 'toValues1) "NonEmptyMap cannot be empty"


toValuesNE :: Map k a -> (a, [a])
toValuesNE (Map dict) =
  case Map.elems dict of
    v:vs -> (v,vs)
    []   -> $(Crash.crash 'toValuesNE) "NonEmptyMap cannot be empty"


fromDict :: Map.Map k a -> Maybe (Map k a)
fromDict dict =
  case dict of
    Map.Bin _ _ _ _ _ -> Just (Map dict)
    Map.Tip           -> Nothing


fromList :: (Ord k) => (k,a) -> [(k,a)] -> Map k a
fromList pair pairs =
  Map $ Map.fromList (pair:pairs)


fromDistinctAscList :: (Ord k) => [(k,a)] -> (k,a) -> Map k a
fromDistinctAscList pairs (k,v) =
  Map $ Map.insert k v $ Map.fromDistinctAscList pairs



-- MAX VALUES


getMaxKey :: Map k a -> k
getMaxKey (Map dict) =
  case dict of
    Map.Bin _ k _ _ r -> loop k r
    Map.Tip           -> $(Crash.crash 'getMaxKey) "NonEmptyMap cannot be empty"
  where
    loop key tree =
      case tree of
        Map.Tip           -> key
        Map.Bin _ k _ _ r -> loop k r


maxView :: (Ord k) => Map k a -> (Maybe (Map k a), k, a)
maxView (Map dict) =
  case dict of
    Map.Bin 1 k v _ _ ->
      (Nothing, k, v)

    Map.Bin _ k v l r ->
      case maxViewHelp k v l r of
        Max tree kmax vmax ->
          case tree of
            Map.Tip               -> ( Nothing     , kmax, vmax )
            d@(Map.Bin _ _ _ _ _) -> ( Just (Map d), kmax, vmax )

    Map.Tip ->
      $(Crash.crash 'maxView) "NonEmptyMap cannot be empty"


data Max k a =
  Max !(Map.Map k a) !k a


maxViewHelp :: (Ord k) => k -> a -> Map.Map k a -> Map.Map k a -> Max k a
maxViewHelp =
    loop
  where
    loop k v l r =
      case r of
        Map.Tip               -> Max l k v
        Map.Bin _ kr xr lr rr ->
          case loop kr xr lr rr of
            Max r' km vm -> Max (Map.balanceL k v l r') km vm



-- ENCODE/DECODE


encode8  :: (k -> E.Builder) -> (a -> E.Builder) -> Map k a -> E.Builder
encode16 :: (k -> E.Builder) -> (a -> E.Builder) -> Map k a -> E.Builder
encode32 :: (k -> E.Builder) -> (a -> E.Builder) -> Map k a -> E.Builder

encode8  eK eV (Map dict) = E.dict8  eK eV dict
encode16 eK eV (Map dict) = E.dict16 eK eV dict
encode32 eK eV (Map dict) = E.dict32 eK eV dict


decode8  :: D.Decoder k -> D.Decoder a -> D.Decoder (Map k a)
decode16 :: D.Decoder k -> D.Decoder a -> D.Decoder (Map k a)
decode32 :: D.Decoder k -> D.Decoder a -> D.Decoder (Map k a)

decode8  dK dV = Map <$> D.dict8  dK dV
decode16 dK dV = Map <$> D.dict16 dK dV
decode32 dK dV = Map <$> D.dict32 dK dV


