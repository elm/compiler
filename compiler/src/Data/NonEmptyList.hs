module Data.NonEmptyList
  ( List(..)
  , singleton
  , toList
  , sortBy
  --
  , eList32, dList32
  )
  where


import qualified Data.List as List

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E



-- LIST


data List a =
  List a [a]


singleton :: a -> List a
singleton a =
  List a []


toList :: List a -> [a]
toList (List x xs) =
  x:xs



-- INSTANCES


instance Functor List where
  fmap func (List x xs) = List (func x) (map func xs)


instance Traversable List where
  traverse func (List x xs) = List <$> func x <*> traverse func xs


instance Foldable List where
  foldr step state (List x xs) = step x (foldr step state xs)
  foldl step state (List x xs) = foldl step (step state x) xs
  foldl1 step      (List x xs) = foldl step x xs



-- SORT BY


sortBy :: (Ord b) => (a -> b) -> List a -> List a
sortBy toRank (List x xs) =
  let
    comparison a b =
      compare (toRank a) (toRank b)
  in
  case List.sortBy comparison xs of
    [] ->
      List x []

    y:ys ->
      case comparison x y of
        LT -> List x (y:ys)
        EQ -> List x (y:ys)
        GT -> List y (List.insertBy comparison x ys)



-- BINARY


eList32 :: (a -> E.Builder) -> List a -> E.Builder
eList32 enc (List x xs) =
  enc x <> E.list32 enc xs


dList32 :: D.Decoder a -> D.Decoder (List a)
dList32 dec =
  do  x  <- dec
      xs <- D.list32 dec
      return $ List x xs


