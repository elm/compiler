{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Suggest
  ( distance
  , sort
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
