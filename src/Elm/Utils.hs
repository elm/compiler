{-# OPTIONS_GHC -Wall #-}
module Elm.Utils
  ( (|>), (<|)
  , drawCycle
  , nearbyNames
  , Entry(..)
  , parseEntry
  )
  where


import Parse.Repl (Entry(..), parseEntry)
import Reporting.Helpers (drawCycle, nearbyNames)



-- PIPES


{-| Forward function application `x |> f == f x`. This function is useful
for avoiding parenthesis and writing code in a more natural way.
-}
(|>) :: a -> (a -> b) -> b
x |> f = f x


{-| Backward function application `f <| x == f x`. This function is useful for
avoiding parenthesis.
-}
(<|) :: (a -> b) -> a -> b
f <| x = f x


infixr 0 <|
infixl 0 |>

