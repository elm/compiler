module Functions exposing (..)

identity x = x

add a b = a + b

apply f x = f x

withLet x =
  let
    y = x + 1
  in
  y * 2

lambda = \x -> x + 1

annotated : Int -> Int
annotated x = x + 1
