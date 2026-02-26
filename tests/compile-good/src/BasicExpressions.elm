module BasicExpressions exposing (..)

add : Int -> Int -> Int
add a b = a + b

greet : String -> String
greet name = "Hello, " ++ name

isEven : Int -> Bool
isEven n = modBy 2 n == 0

double : Float -> Float
double x = x * 2.0
