{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Helpers where

import Data.Function (on)
import qualified Data.List as List
import qualified Text.EditDistance as Dist

import Elm.Utils ((|>))


-- NEARBY NAMES

nearbyNames :: (a -> String) -> a -> [a] -> [a]
nearbyNames format name names =
  let editDistance =
        if length (format name) < 3 then 1 else 2
  in
      names
        |> map (\x -> (distance (format name) (format x), x))
        |> List.sortBy (compare `on` fst)
        |> filter ( (<= editDistance) . abs . fst )
        |> map snd


distance :: String -> String -> Int
distance x y =
  Dist.restrictedDamerauLevenshteinDistance Dist.defaultEditCosts x y


maybeYouWant :: [String] -> String
maybeYouWant suggestions =
  case suggestions of
    [] ->
        ""

    _:_ ->
        "Maybe you want one of the following?\n"
        ++ concatMap ("\n    "++) (take 4 suggestions)

