module Elm.Magnitude
  ( Magnitude(..)
  , toChars
  )
  where



-- MAGNITUDE


data Magnitude
  = PATCH
  | MINOR
  | MAJOR
  deriving (Eq, Ord)


toChars :: Magnitude -> String
toChars magnitude =
  case magnitude of
    PATCH -> "PATCH"
    MINOR -> "MINOR"
    MAJOR -> "MAJOR"
