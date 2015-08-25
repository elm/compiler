module ArrayBench where

import Array
import Random
import Debug exposing (crash)



randomArray 
  :  Random.Generator a 
  -> Int 
  -> Random.Seed 
  -> Array.Array a
randomArray gen n initSeed =
  let
    f n (seed, accum) =
      let
        (nextToAdd, newSeed) = Random.generate gen seed
        newAccum = Array.push nextToAdd accum
      in (newSeed, newAccum)
  in 
    snd <| List.foldl f (initSeed, Array.empty) [1 .. n]

meanAndStddev 
  :  ((Float -> (Float, Float) -> (Float, Float)) -> (Float, Float) -> Array.Array Float -> (Float, Float)) 
  -> Array.Array Float
  -> (Float, Float)
meanAndStddev foldFn ourArray =
  let 
    --ourArray = randomArray n (Random.initialSeed 23)
    (theSum, sumSq) = foldFn (\x (sum, sumSq) -> (x + sum, (x^2) + sumSq) ) (0,0) ourArray
  in
    (theSum, sqrt(theSum*theSum - sumSq))
    

foldrMeanStddev = meanAndStddev Array.foldr


foldlMeanStddev = meanAndStddev Array.foldl


customFoldrMeanStddev = meanAndStddev myFoldr 


customFoldlMeanStddev = meanAndStddev myFoldl


updateNRandomIndices
  :  (Float -> Float)
  -> List Int
  -> Array.Array Float
  -> Array.Array Float
updateNRandomIndices f indexList startList = 
  List.foldr 
  (\ i oldList -> updateN i f oldList)
  startList indexList


randomLookups : List Int -> Array.Array Float -> List (Maybe Float)
randomLookups indices arr =
  List.map (\i -> Array.get i arr) indices


myFoldl : (a -> b -> b) -> b -> Array.Array a -> b
myFoldl f init arr = 
  let
    len = Array.length arr
    fromJust (Just x) = x 
    helper init i = 
      if (i >= len)
      then init
      else
        helper (f (fromJust <| Array.get i arr) init) (i+1)
  in helper init 0


myFoldr : (a -> b -> b) -> b -> Array.Array a -> b
myFoldr f init arr = 
  let
    len = Array.length arr
    fromJust (Just x) = x 
    helper init i = 
      if (i <= 0)
      then init
      else
        helper (f (fromJust <| Array.get i arr) init) (i-1)
  in helper init (len-1)

  
updateN : Int -> (a -> a) -> Array.Array a -> Array.Array a
updateN n f arr = 
  case Array.get n arr of
    Just x -> Array.set n (f x) arr
    _ -> crash "Array out of bounds"