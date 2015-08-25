module TupleBench where

type alias BigTuple = 
  ( Float 
  , Float
  , Float
  , Float
  , Float
  , Float
  , Float
  , Float
  , Float
  )
  
defaultTuple = 
  (1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)
  
getField (x1, x2, x3, x4, x5, x6, x7, x8, x9) i = 
  case (i % 9) of
    1 -> 
      x1
    2 -> 
      x2
    3 -> 
      x3
    4 -> 
      x4
    5 -> 
      x5
    6 -> 
      x6
    7 -> 
      x7
    8 -> 
      x8
    0 -> 
      x9
      

sumRandomElems : List Int -> Float
sumRandomElems randomInts = 
  List.foldr (\i sumSoFar -> (getField defaultTuple i) + sumSoFar ) 0.0 randomInts