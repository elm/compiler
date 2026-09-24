{-# LANGUAGE Rank2Types #-}
module Data.Dups
  ( Dict(..)
  , none
  , one
  , insert
  , union
  , unions
  --
  , Dict1
  , size1
  , one1
  , insert1
  --
  , Detector(..)
  , detect
  , detect_
  , detect1
  , detects
  )
  where


import Data.Foldable (sequenceA_)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Utils as Map
import qualified Data.NonEmptyMap as NEM
import qualified Data.OneOrMore as OOM



-- DICT


newtype Dict key region value =
  Dict (Map.Map key (Entries region value))


data Entries r v
  = One r v
  | More r r



-- DICT HELPERS


none :: Dict k r a
none =
  Dict Map.empty


one :: k -> r -> a -> Dict k r a
one k r a =
  Dict $ Map.singleton k (One r a)


insert :: (Ord k) => k -> r -> a -> Dict k r a -> Dict k r a
insert k r a (Dict dict) =
  Dict $ Map.insertWith (\new old -> more old new) k (One r a) dict


union :: (Ord k) => Dict k r a -> Dict k r a -> Dict k r a
union (Dict a) (Dict b) =
  Dict $ Map.unionWith more a b


unions :: (Ord k) => [Dict k r a] -> Dict k r a
unions dicts =
  case dicts of
    []   -> none
    d:ds -> List.foldl' union d ds


more :: Entries r v -> Entries r v -> Entries r v
more e1 e2 =
  case e1 of
    More _ _ -> e1
    One r1 _ ->
      case e2 of
        One  r2 _ -> More r1 r2
        More r2 _ -> More r1 r2



-- DICT 1


newtype Dict1 key region value =
  Dict1 (NEM.Map key (Entries region value))


size1 :: Dict1 k r v -> Int
size1 (Dict1 dict) =
  NEM.size dict


one1 :: k -> r -> a -> Dict1 k r a
one1 k r a =
  Dict1 $ NEM.singleton k (One r a)


insert1 :: (Ord k) => k -> r -> a -> Dict1 k r a -> Dict1 k r a
insert1 k r a (Dict1 dict) =
  Dict1 $ NEM.insertWith (\new old -> more old new) k (One r a) dict



-- DETECTOR


newtype Detector x a =
  Detector (forall r. (a -> r) -> (OOM.OneOrMore x -> r) -> r)


throw :: x -> Detector x a
throw x =
  Detector $ \_ err ->
    err (OOM.one x)


instance Functor (Detector x) where
  {-# INLINE fmap #-}
  fmap func (Detector k) =
    Detector $ \ok err ->
      k (ok . func) err


instance Applicative (Detector x) where
  {-# INLINE pure #-}
  pure a =
    Detector $ \ok _ ->
      ok a

  {-# INLINE (<*>) #-}
  (<*>) (Detector kFunc) (Detector kArg) =
    Detector $ \ok err ->
      let
        ok1 func = kArg (ok . func) err
        err1 xs  = kArg (\_ -> err xs) (\ys -> err (OOM.more xs ys))
      in
      kFunc ok1 err1



-- DETECT


detect :: (k -> r -> a -> v) -> (k -> r -> r -> x) -> Dict k r a -> Detector x (Map.Map k v)
detect ok err (Dict dict) =
  Map.traverseWithKey (dedup ok err) dict


detect1 :: (k -> r -> a -> v) -> (k -> r -> r -> x) -> Dict1 k r a -> Detector x (NEM.Map k v)
detect1 ok err (Dict1 dict) =
  NEM.traverseWithKey (dedup ok err) dict


detect_ :: (k -> r -> r -> x) -> Dict k r a -> Detector x ()
detect_ err (Dict dict) =
  Map.traverseWithKey_ (dedup_ err) dict


detects :: (Ord k) => (k -> r -> a1 -> v1) -> (k -> r -> a2 -> v2) -> (k -> r -> r -> x) -> Dict k r a1 -> Dict k r a2 -> Detector x (Map.Map k v1, Map.Map k v2)
detects ok1 ok2 err dups1@(Dict dict1) dups2@(Dict dict2) =
  sequenceA_ (Map.intersectionWithKey errs dict1 dict2)
  *>
  (
    (,)
      <$> detect ok1 err dups1
      <*> detect ok2 err dups2
  )
  where
    errs k e1 e2 =
      case e1 of
        More x y -> throw (err k x y)
        One x _ ->
          case e2 of
            One  y _ -> throw (err k x y)
            More y _ -> throw (err k x y)



-- DETECT HELPERS


dedup :: (k -> r -> a -> v) -> (k -> r -> r -> x) -> k -> Entries r a -> Detector x v
dedup ok err key entries =
  case entries of
    One  r a -> pure (ok key r a)
    More x y -> throw (err key x y)


dedup_ :: (k -> r -> r -> x) -> k -> Entries r a -> Detector x ()
dedup_ err key entries =
  case entries of
    One  _ _ -> pure ()
    More x y -> throw (err key x y)
