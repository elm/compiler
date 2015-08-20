module DictBench where

import Dict
import Random
import Debug exposing (crash)



randomDict 
  :  Random.Generator a 
  -> Int 
  -> Random.Seed 
  -> Dict.Dict Int a
randomDict gen n initSeed =
  let
    f n (seed, accum) =
      let
        (nextToAdd, newSeed) = Random.generate gen seed
        newAccum = Dict.insert n nextToAdd accum
      in (newSeed, newAccum)
  in 
    snd <| List.foldl f (initSeed, Dict.empty) [1 .. n]

meanAndStddev 
  :  ((Int -> Float -> (Float, Float) -> (Float, Float)) -> (Float, Float) -> Dict.Dict Int Float -> (Float, Float)) 
  -> Dict.Dict Int Float
  -> (Float, Float)
meanAndStddev foldFn ourDict =
  let 
    --ourDict = randomDict n (Random.initialSeed 23)
    (theSum, sumSq) = foldFn (\_ x (sum, sumSq) -> (x + sum, (x^2) + sumSq) ) (0,0) ourDict
  in
    (theSum, sqrt(theSum*theSum - sumSq))
    

foldrMeanStddev = meanAndStddev Dict.foldr


foldlMeanStddev = meanAndStddev Dict.foldl


updateNRandomIndices
  :  (Float -> Float)
  -> List Int
  -> Dict.Dict Int Float
  -> Dict.Dict Int Float
updateNRandomIndices f indexList startList = 
  List.foldr 
  (\ i oldList -> updateN i f oldList)
  startList indexList


randomLookups : List Int -> Dict.Dict Int Float -> List (Maybe Float)
randomLookups indices arr =
  List.map (\i -> Dict.get i arr) indices


  
updateN : Int -> (a -> a) -> Dict.Dict Int a -> Dict.Dict Int a
updateN n f arr = 
  case Dict.get n arr of
    Just x -> Dict.insert n (f x) arr
    _ -> crash "Dict out of bounds"