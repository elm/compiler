module IfBench where

tailRecursiveIfFn : Int -> Float
tailRecursiveIfFn i = 
  if 
    | i < 0 ->
      10.0
    | i == 0 ->
      20.0
    | i > 0 && i < 10 ->
      30.0
    | otherwise ->
      tailRecursiveIfFn (i - (1 + (i % 5 ) ) )
    
    
manyConditionFunction i =
  if
    | i <= 0 ->
      30.0 + (toFloat i)
    | i > 100 && (i * i) % 20 == 7 ->
      24.3 + (toFloat i)
    | i > 100 && (i % 3 == 1) ->
      3.33 + (toFloat i)
    | (i == 152) ->
      29.0 + (toFloat i)
    | (i + (i*i) > 200 && (i + (i*i) < 300)) -> 
      32.9 + (toFloat i)
    | i == 99 ->
      93.0 + (toFloat i)
    | (i*i % 17 == 13 ) ->
      9.0 + (toFloat i)
    | (i == 101) ->
      39.0 + (toFloat i)
    | (i == 102) ->
      38.0 + (toFloat i)
    | (i == 103) ->
      49.0 + (toFloat i)
    | (i == 104) ->
      59.0 + (toFloat i)
    | (i == 105) ->
      69.0 + (toFloat i)
    | otherwise ->
      toFloat i
  
  
tailRecSum intList = 
  List.foldr (\i sumSoFar -> sumSoFar + tailRecursiveIfFn i ) 0.0 intList  
      
manyConditionalSum intList = 
  List.foldr (\i sumSoFar -> sumSoFar + manyConditionFunction i ) 0.0 intList
    