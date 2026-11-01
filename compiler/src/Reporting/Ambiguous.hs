module Reporting.Ambiguous
  ( Ambiguous
  , init
  , add
  , merge
  , toList
  )
  where


import Prelude hiding (init)
import qualified Data.Set as Set



-- AMBIGUOUS
--
-- The Set is always guaranteed to have two or more values in it.
-- Otherwise it is a compiler bug!
--
-- PERF this is not always an error, so does it make sense to have a cheaper
-- representation for add/merge?


newtype Ambiguous a =
  Ambiguous (Set.Set a)



-- INIT


init :: (Ord a) => a -> a -> Ambiguous a
init x y =
  Ambiguous (Set.insert x (Set.singleton y))



-- ADD


add :: (Ord a) => a -> Ambiguous a -> Ambiguous a
add z (Ambiguous set) =
  Ambiguous (Set.insert z set)



-- MERGE


merge :: (Ord a) => Ambiguous a -> Ambiguous a -> Ambiguous a
merge (Ambiguous s1) (Ambiguous s2) =
  Ambiguous (Set.union s1 s2)



-- TO LIST


toList :: (Ord a) => Ambiguous a -> [a]
toList (Ambiguous set) =
  Set.toList set
