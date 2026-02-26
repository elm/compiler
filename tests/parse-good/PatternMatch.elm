module PatternMatch exposing (..)

type Color = Red | Green | Blue

describe color =
  case color of
    Red -> "red"
    Green -> "green"
    Blue -> "blue"

first tuple =
  case tuple of
    (a, _) -> a

headOf list =
  case list of
    [] -> Nothing
    x :: _ -> Just x
