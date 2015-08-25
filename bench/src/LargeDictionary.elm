module LargeDictionary where


import Dict
import Random
import Signal
import Time
import Graphics.Element


maxSize = 10000000


randomGen =
  Random.int (-1 * maxSize) (maxSize)


addNToDictWithSeed 
  :  Int 
  -> Random.Seed 
  -> Dict.Dict Int Int 
  -> Dict.Dict Int Int
addNToDictWithSeed n seed accum = 
  case n of
    0 -> accum
    _ -> 
      let
        (nextToAdd, newSeed) = Random.generate randomGen seed
        newAccum = Dict.insert nextToAdd nextToAdd accum
      in addNToDictWithSeed (n-1) newSeed newAccum


addNToDictAndSum numToInsert = 
  addNToDictWithSeed numToInsert (Random.initialSeed 23) Dict.empty