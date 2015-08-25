module SmallTupleBench where

type alias SmallTuple = 
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
  (1.0, 2.0, 3.0)
  
getField (x1, x2, x3) i = 
  case (i % 3) of
    1 -> 
      x1
    2 -> 
      x2
    0 -> 
      x3
      

sumRandomElems : List Int -> Float
sumRandomElems randomInts = 
  List.foldr (\i sumSoFar -> (getField defaultTuple i) + sumSoFar ) 0.0 randomInts