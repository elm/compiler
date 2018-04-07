{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Suggest
  ( findPotentialTypos, findTypoPairs, vetTypos
  , nearbyNames, distance
  )
  where


import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Text.EditDistance as Dist



-- FIND TYPOS


findPotentialTypos :: [String] -> String -> [String]
findPotentialTypos knownNames badName =
  filter (\name -> distance badName name == 1) knownNames


findTypoPairs :: [String] -> [String] -> [(String, String)]
findTypoPairs leftOnly rightOnly =
  let
    veryNear leftName =
      map ((,) leftName) (findPotentialTypos rightOnly leftName)
  in
    concatMap veryNear leftOnly


vetTypos :: [(String, String)] -> Maybe (Set.Set String, Set.Set String)
vetTypos potentialTypos =
  let
    tallyNames (ln, rn) (lc, rc) =
      ( Map.insertWith (+) ln 1 lc
      , Map.insertWith (+) rn 1 rc
      )

    (leftCounts, rightCounts) =
      foldr tallyNames (Map.empty, Map.empty) potentialTypos

    acceptable :: Map.Map String Int -> Bool
    acceptable counts =
      not (Map.null counts)
      &&
      Map.foldr (\n unique -> n < 2 && unique) True counts
  in
    if acceptable leftCounts && acceptable rightCounts then
        Just (Map.keysSet leftCounts, Map.keysSet rightCounts)

    else
        Nothing



-- NEARBY NAMES


nearbyNames :: (a -> String) -> a -> [a] -> [a]
nearbyNames toString value possibleValues =
  let
    name =
      toString value

    editDistance =
      if length name < 3 then 1 else 2

    getDistance pValue =
      let
        dist = abs (distance name (toString pValue))
      in
      if dist <= editDistance then Just (dist, pValue) else Nothing
  in
  map snd $
    List.sortBy (compare `on` fst) $
      Maybe.mapMaybe getDistance possibleValues


distance :: String -> String -> Int
distance x y =
  Dist.restrictedDamerauLevenshteinDistance Dist.defaultEditCosts x y
