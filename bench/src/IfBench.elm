module IfBench where


tailRecursiveIfFn : Int -> Float
tailRecursiveIfFn i =
  if i < 0 then
      10.0

  else if i == 0 then
      20.0

  else if i > 0 && i < 10 then
      30.0

  else
      tailRecursiveIfFn (i - (1 + (i % 5)))


manyConditionFunction i =
  if i <= 0 then
      30.0 + (toFloat i)

  else if i > 100 && (i * i) % 20 == 7 then
      24.3 + (toFloat i)

  else if i > 100 && (i % 3 == 1) then
      3.33 + (toFloat i)

  else if i == 152 then
      29.0 + (toFloat i)

  else if i + (i*i) > 200 && (i + (i*i) < 300) then
      32.9 + (toFloat i)

  else if i == 99 then
      93.0 + (toFloat i)

  else if i*i % 17 == 13 then
      9.0 + (toFloat i)

  else if i == 101 then
      39.0 + (toFloat i)

  else if i == 102 then
      38.0 + (toFloat i)

  else if i == 103 then
      49.0 + (toFloat i)

  else if i == 104 then
      59.0 + (toFloat i)

  else if i == 105 then
      69.0 + (toFloat i)

  else if i == 106 then
      39.0 + (toFloat i)

  else if i == 107 then
      38.0 + (toFloat i)

  else if i == 108 then
      49.0 + (toFloat i)

  else if i == 109 then
      59.0 + (toFloat i)

  else if i == 110 then
      69.0 + (toFloat i)

  else if i == 111 then
      39.0 + (toFloat i)

  else if i == 115 then
      69.0 + (toFloat i)

  else if otherwise then
      toFloat i


fewConditionFunction i =
  if i <= 0 then
      30.0 + (toFloat i)

  else if i == 104 then
      59.0 + (toFloat i)

  else if i == 105 then
      69.0 + (toFloat i)

  else
      toFloat i


tailRecSum intList =
  List.foldr (\i sumSoFar -> sumSoFar + tailRecursiveIfFn i ) 0.0 intList


manyConditionalSum intList =
  List.foldr (\i sumSoFar -> sumSoFar + manyConditionFunction i ) 0.0 intList


fewConditionalSum intList =
  List.foldr (\i sumSoFar -> sumSoFar + fewConditionFunction i ) 0.0 intList
