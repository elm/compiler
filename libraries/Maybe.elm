module Maybe where

{-| Represents an optional value. Maybe it is there, maybe it is not.

# Type and Constructors
@docs Maybe

# Examining Maybes
@docs isJust, isNothing

# Transforming Maybes
@docs map, maybe

# Maybes and Lists
@docs justs
-}

import Basics (not, (.))
import List (foldr)

{-| The `Maybe` datatype. Useful when a computation may or may not
result in a value (e.g. logarithm is defined only for positive numbers).
-}
data Maybe a = Just a | Nothing

{-| Apply a function to the contents of a `Just`, or return the default when
given `Nothing`.
-}
maybe : b -> (a -> b) -> Maybe a -> b
maybe b f m = case m of
                Just v  -> f v
                Nothing -> b

{-| Apply a function to the contents of a `Maybe`, returning either the
transformed `Just` or `Nothing`.
-}
map : (a -> b) -> Maybe a -> Maybe b
map f m = case m of
            Just v -> Just (f v)
            Nothing -> Nothing

{-| Check if constructed with `Just`.
-}
isJust : Maybe a -> Bool
isJust = maybe False (\_ -> True)

{-| Check if constructed with `Nothing`.
-}
isNothing : Maybe a -> Bool
isNothing = not . isJust

cons mx xs = maybe xs (\x -> x :: xs) mx

{-| Filters out `Nothing`s from a list and extracts the remaining values.
-}
justs : [Maybe a] -> [a]
justs = foldr cons []
