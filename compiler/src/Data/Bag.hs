{-# OPTIONS_GHC -Wall #-}
module Data.Bag
  ( Bag(..)
  , empty
  , one
  , append
  , map
  , toList
  , fromList
  )
  where


import Prelude hiding (map)
import qualified Data.List as List



-- BAGS


data Bag a
  = Empty
  | One a
  | Two (Bag a) (Bag a)



-- HELPERS


empty :: Bag a
empty =
  Empty


one :: a -> Bag a
one =
  One


append :: Bag a -> Bag a -> Bag a
append left right =
  case (left, right) of
    (other, Empty) ->
      other

    (Empty, other) ->
      other

    (_, _) ->
      Two left right



-- MAP


map :: (a -> b) -> Bag a -> Bag b
map func bag =
  case bag of
    Empty ->
      Empty

    One a ->
      One (func a)

    Two left right ->
      Two (map func left) (map func right)



-- TO LIST


toList :: Bag a -> [a]
toList bag =
  toListHelp bag []


toListHelp :: Bag a -> [a] -> [a]
toListHelp bag list =
  case bag of
    Empty ->
      list

    One x ->
      x : list

    Two a b ->
      toListHelp a (toListHelp b list)



-- FROM LIST


fromList :: (a -> b) -> [a] -> Bag b
fromList func list =
  case list of
    [] ->
      Empty

    first : rest ->
      List.foldl' (add func) (One (func first)) rest


add :: (a -> b) -> Bag b -> a -> Bag b
add func bag value =
  Two (One (func value)) bag
