module ListBench where

import Random



randomList 
  :  Random.Generator a
  -> Int 
  -> Random.Seed 
  -> List a
randomList gen n initSeed =
  let
    f n (seed, accum) =
      let
        (nextToAdd, newSeed) = Random.generate gen seed
        newAccum = nextToAdd :: accum
      in (newSeed, newAccum)
  in 
    snd <| List.foldl f (initSeed, []) [1 .. n]

meanAndStddev 
  :  ((Float -> (Float, Float) -> (Float, Float)) -> (Float, Float) -> List Float -> (Float, Float)) 
  -> List Float
  -> (Float, Float)
meanAndStddev foldFn ourList =
  let 
    --ourList = addNToListWithSeed n (Random.initialSeed 23)
    (theSum, sumSq) = foldFn (\x (sum, sumSq) -> (x + sum, (x^2) + sumSq) ) (0,0) ourList
  in
    (theSum, sqrt(theSum*theSum - sumSq))
    

foldrMeanStddev = meanAndStddev List.foldr


foldlMeanStddev = meanAndStddev List.foldl


customFoldrMeanStddev = meanAndStddev myFoldr 


customFoldlMeanStddev = meanAndStddev myFoldl


updateNRandomIndices
  :  (Float -> Float)
  -> List Int
  -> List Float
  -> List Float
updateNRandomIndices f indexList startList = 
  List.foldr 
  (\ i oldList -> updateN i f oldList)
  startList indexList
  


randomLookups : List Int -> List Float -> List (Maybe Float)
randomLookups indices ourList =
  let
    getNth i l = 
      case (i, l) of
        (_, []) -> Nothing
        (0, x :: _) -> Just x
        (i, _ :: xs ) -> getNth i xs 
  in List.map (\i -> getNth i ourList) indices


myFoldl : (a -> b -> b) -> b -> List a -> b
myFoldl f init l = 
  case l of
    [] -> init
    (x :: xs) -> myFoldl f (f x init) xs 


myFoldr : (a -> b -> b) -> b -> List a -> b
myFoldr f init l =
  case l of
    [] -> init
    (x :: xs) -> f x (myFoldr f init xs)
    

updateN : Int -> (a -> a) -> List a -> List a
updateN n f list = 
  List.indexedMap (\index value -> if index == n then f value else value) list    