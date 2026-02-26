module PatternMatching exposing (..)

type Shape
  = Circle Float
  | Rectangle Float Float
  | Triangle Float Float Float

describe : Shape -> String
describe shape =
  case shape of
    Circle r ->
      "circle with radius " ++ String.fromFloat r

    Rectangle w h ->
      "rectangle " ++ String.fromFloat w ++ "x" ++ String.fromFloat h

    Triangle a b c ->
      "triangle"

safeHead : List a -> Maybe a
safeHead list =
  case list of
    [] -> Nothing
    x :: _ -> Just x
