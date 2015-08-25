module PatternMatchBench where

type NestedStructure = 
    Nest Float (NestedStructure)
  | End Float
  

intToNest : Int -> NestedStructure
intToNest i = 
  let
    helper i accum =
      case (i % 20) of
        0 -> 
          accum
        
        _ -> 
          helper (i-1) <| Nest (toFloat <| i * i) accum 
  in
    helper i (End <| toFloat i)
    
deepMatchingFunction : NestedStructure -> Float
deepMatchingFunction struct = 
  case struct of
    (Nest x1 (Nest x2 (Nest x3 (Nest x4 (Nest x5 (Nest x6 (Nest x7 (Nest x8 (Nest x9 (Nest x10 (Nest x11 (Nest x12 (Nest x13 (Nest x14 (Nest x15 (Nest x16 (Nest x17 (End x18 ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ->
      x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18
    
    Nest x _ -> 
      x
    
    End x -> 
      x
      

wildcardMatchingFunction : NestedStructure -> Float
wildcardMatchingFunction struct = 
  case struct of
    (Nest x (Nest _ (Nest _ (Nest _ (Nest _ (Nest _ (Nest _ (Nest _ (Nest _ (Nest _ (Nest _ (Nest _ (Nest _ (Nest _ (Nest _ (Nest _ (Nest _ (End y ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ->
      x + y + x + y + x + y + x + y + x + y + x + y + x + y + x + y + x + y
    
    Nest x _ -> 
      x
    
    End x -> 
      x
      
      
shallowMatchingFunction : NestedStructure -> Float
shallowMatchingFunction  struct = 
  case struct of
    (Nest x1 (Nest x2 (Nest x3 (End x4) ) ) ) ->
      x1 + x2 + x3 + x4 + x1 + x2 + x3 + x4 + x1 + x2 + x3 + x4 + x1 + x2 + x3 + x4 + x1 + x2
    
    Nest x _ -> 
      x
    
    End x -> 
      x
      
sumStructures : (NestedStructure -> Float) -> List NestedStructure -> Float
sumStructures f inList =
  List.foldr (\x sumSoFar -> sumSoFar + f x) 0.0 inList
  
  
deepMatchSum = sumStructures deepMatchingFunction


shallowMatchSum = sumStructures shallowMatchingFunction


wildcardMatchSum = sumStructures wildcardMatchingFunction