{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Reporting.Suggest
  ( distance
  , sort
  , rank
  , best
  )
  where


import qualified Data.Char as Char
import qualified Data.List as List
import qualified Text.EditDistance as Dist



-- DISTANCE


distance :: String -> String -> Int
distance x y =
  Dist.restrictedDamerauLevenshteinDistance Dist.defaultEditCosts x y



-- SORT


sort :: String -> (a -> String) -> [a] -> [a]
sort target toString values =
  List.sortOn (distance (toLower target) . toLower . toString) values


toLower :: String -> String
toLower string =
  map Char.toLower string



-- RANK


rank :: String -> (a -> String) -> [a] -> [(Int,a)]
rank target toString values =
  let
    toRank v =
      distance (toLower target) (toLower (toString v))

    addRank v =
      (toRank v, v)
  in
  List.sortOn fst (map addRank values)



-- BEST


best :: (a -> String) -> [a] -> [a] -> Maybe (a, a)
best toString values1 values2 =
  case List.map (\v -> (v, toLower (toString v))) values1 of
    [] ->
      Nothing

    pairs1@(p1:_) ->
      case List.map (\v -> (v, toLower (toString v))) values2 of
        [] ->
          Nothing

        pairs2@(p2:_) ->
          let
            (_, before, after) =
              List.foldl' (\s p -> bestRow p pairs2 s) (initialize p1 p2) pairs1
          in
          Just (before, after)
  where
    bestRow pair1 pairs2 state =
      List.foldl' (bestCol pair1) state pairs2

    bestCol (v1,lower1) state@(dist, _, _) (v2,lower2) =
      let !d = distance lower1 lower2 in
      if d < dist then (d, v1, v2) else state

    initialize (v1,lower1) (v2,lower2) =
      ( distance lower1 lower2
      , v1
      , v2
      )
