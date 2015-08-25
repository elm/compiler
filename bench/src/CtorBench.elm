module CtorBench where

type ManyCtors = 
    MyLongNamedCtor1 Float Float Float
  | MyLongNamedCtor2 Float Float
  | MyLongNamedCtor3 Float Float Float
  | MyLongNamedCtor4 Float Float
  | MyLongNamedCtor5 Float Float Float
  | MyLongNamedCtor6 Float Float
  | MyLongNamedCtor7 Float Float Float
  | MyLongNamedCtor8 Float Float
  | MyLongNamedCtor9 Float Float Float
  | MyLongNamedCtor10 Float Float
  | MyLongNamedCtor11 Float Float Float
  | MyLongNamedCtor12 Float Float
  | MyLongNamedCtor13 Float Float Float
  | MyLongNamedCtor14 Float Float
  | MyLongNamedCtor15 Float Float Float
  | MyLongNamedCtor16 Float Float
  | MyLongNamedCtor17 Float Float Float
  | MyLongNamedCtor18 Float Float
  | MyLongNamedCtor19 Float Float Float
  | MyLongNamedCtor20 Float Float
  

getFieldSum someData = 
  case someData of
    MyLongNamedCtor1 x y z ->
      x + y + z
    
    MyLongNamedCtor3 x y z ->
      x + y + z
    
    MyLongNamedCtor5 x y z ->
      x + y + z
    
    MyLongNamedCtor7 x y z ->
      x + y + z
    
    MyLongNamedCtor9 x y z ->
      x + y + z
    
    MyLongNamedCtor11 x y z ->
      x + y + z
    
    MyLongNamedCtor13 x y z ->
      x + y + z
    
    MyLongNamedCtor15 x y z ->
      x + y + z
    
    MyLongNamedCtor17 x y z ->
      x + y + z
    
    MyLongNamedCtor19 x y z ->
      x + y + z
    
    MyLongNamedCtor20 x y ->
      x + y
    
    MyLongNamedCtor18 x y ->
      x + y
      
    MyLongNamedCtor16 x y ->
      x + y
      
    MyLongNamedCtor14 x y ->
      x + y
    
    MyLongNamedCtor12 x y ->
      x + y
    
    MyLongNamedCtor10 x y ->
      x + y
    
    MyLongNamedCtor8 x y ->
      x + y
    
    MyLongNamedCtor6 x y ->
      x + y
    
    MyLongNamedCtor4 x y ->
      x + y
    
    MyLongNamedCtor2 x y ->
      x + y
      

makeRandomData i = 
  case (i % 20) of
    0 -> 
      MyLongNamedCtor20 (toFloat i) (toFloat <| i*i)
    1 -> 
      MyLongNamedCtor1 (toFloat i) (toFloat <| i*i) (toFloat <| i*i*i)
    2 -> 
      MyLongNamedCtor2 (toFloat i) (toFloat <| i*i)
    3 -> 
      MyLongNamedCtor3 (toFloat i) (toFloat <| i*i) (toFloat <| i*i*i)
    4 -> 
      MyLongNamedCtor4 (toFloat i) (toFloat <| i*i)
    5 -> 
      MyLongNamedCtor5 (toFloat i) (toFloat <| i*i) (toFloat <| i*i*i)
    6 -> 
      MyLongNamedCtor6 (toFloat i) (toFloat <| i*i)
    7 -> 
      MyLongNamedCtor7 (toFloat i) (toFloat <| i*i) (toFloat <| i*i*i)
    8 -> 
      MyLongNamedCtor8 (toFloat i) (toFloat <| i*i)
    9 -> 
      MyLongNamedCtor9 (toFloat i) (toFloat <| i*i) (toFloat <| i*i*i)
    10 -> 
      MyLongNamedCtor10 (toFloat i) (toFloat <| i*i)
    11 -> 
      MyLongNamedCtor11 (toFloat i) (toFloat <| i*i) (toFloat <| i*i*i)
    12 -> 
      MyLongNamedCtor12 (toFloat i) (toFloat <| i*i)
    13 -> 
      MyLongNamedCtor13 (toFloat i) (toFloat <| i*i) (toFloat <| i*i*i)
    14 -> 
      MyLongNamedCtor14 (toFloat i) (toFloat <| i*i)
    15 -> 
      MyLongNamedCtor15 (toFloat i) (toFloat <| i*i) (toFloat <| i*i*i)
    16 -> 
      MyLongNamedCtor16 (toFloat i) (toFloat <| i*i)
    17 -> 
      MyLongNamedCtor17 (toFloat i) (toFloat <| i*i) (toFloat <| i*i*i)
    18 -> 
      MyLongNamedCtor18 (toFloat i) (toFloat <| i*i)
    19 -> 
      MyLongNamedCtor19 (toFloat i) (toFloat <| i*i) (toFloat <| i*i*i)
      

sumFieldsList : List ManyCtors -> Float
sumFieldsList dataList = 
  List.foldr (\val sumSoFar -> (getFieldSum val) + sumSoFar ) 0.0 dataList