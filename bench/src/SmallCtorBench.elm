module SmallCtorBench where

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
  case (i % 10) of
    0 -> 
      MyLongNamedCtor10 (toFloat i) (toFloat <| i*i)
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

      

sumFieldsList : List ManyCtors -> Float
sumFieldsList dataList = 
  List.foldr (\val sumSoFar -> (getFieldSum val) + sumSoFar ) 0.0 dataList