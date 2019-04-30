{-# OPTIONS_GHC -Wall #-}
module Data.OneOrMore
  ( OneOrMore(..)
  , one
  , more
  , map
  , destruct
  , getFirstTwo
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



-- MAP


map :: (a -> b) -> OneOrMore a -> OneOrMore b
map func oneOrMore =
  case oneOrMore of
    One value ->
      One (func value)

    More left right ->
      More (map func left) (map func right)



-- DESTRUCT


destruct :: (a -> [a] -> b) -> OneOrMore a -> b
destruct func oneOrMore =
  destructLeft func oneOrMore []


destructLeft :: (a -> [a] -> b) -> OneOrMore a -> [a] -> b
destructLeft func oneOrMore xs =
  case oneOrMore of
    One x ->
      func x xs

    More a b ->
      destructLeft func a (destructRight b xs)


destructRight :: OneOrMore a -> [a] -> [a]
destructRight oneOrMore xs =
  case oneOrMore of
    One x ->
      x : xs

    More a b ->
      destructRight a (destructRight b xs)



-- GET FIRST TWO


getFirstTwo :: OneOrMore a -> OneOrMore a -> (a,a)
getFirstTwo left right =
  case left of
    One x ->
      (x, getFirstOne right)

    More lleft lright ->
      getFirstTwo lleft lright


getFirstOne :: OneOrMore a -> a
getFirstOne oneOrMore =
  case oneOrMore of
    One x ->
      x

    More left _ ->
      getFirstOne left
