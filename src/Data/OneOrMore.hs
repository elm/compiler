{-# OPTIONS_GHC -Wall #-}
module Data.OneOrMore
  ( OneOrMore(..)
  , one
  , more
  , toList
  , map
  )
  where


import Prelude hiding (map)



-- ONE OR MORE


data OneOrMore a
  = One a
  | More (OneOrMore a) (OneOrMore a)


one :: a -> OneOrMore a
one =
  One


more :: OneOrMore a -> OneOrMore a -> OneOrMore a
more =
  More



-- TO LIST


toList :: OneOrMore a -> [a]
toList oneOrMore =
  toListHelp oneOrMore []


toListHelp :: OneOrMore a -> [a] -> [a]
toListHelp oneOrMore list =
  case oneOrMore of
    One x ->
      x : list

    More a b ->
      toListHelp a (toListHelp b list)



-- MAP


map :: (a -> b) -> OneOrMore a -> OneOrMore b
map func oneOrMore =
  case oneOrMore of
    One value ->
      One (func value)

    More left right ->
      More (map func left) (map func right)
