module Maybe where

{-| Represents an optional value. Maybe it is there, maybe it is not.

# Type and Constructors
@docs Maybe

# Taking Maybes apart
@docs maybe, isJust, isNothing

# Maybes and Lists
@docs justs
-}

import Basics (not, (.))
import List (foldr)

{-| The Maybe datatype. Useful when a computation may or may not
result in a value (e.g. logarithm is defined only for positive
numbers). 
-}
data Maybe a = Just a | Nothing

{-| Apply a function to the contents of a `Maybe`.
Return default when given `Nothing`.
-}
maybe : b -> (a -> b) -> Maybe a -> b
maybe b f m = case m of
                Just v  -> f v
                Nothing -> b

{-| Check if constructed with `Just`. 
-}
isJust : Maybe a -> Bool
isJust = maybe False (\_ -> True)

{-| Check if constructed with `Nothing`.
-}
isNothing : Maybe a -> Bool
isNothing = not . isJust

cons mx xs = maybe xs (\x -> x :: xs) mx

{-| Filters out Nothings and extracts the remaining values.
-}
justs : [Maybe a] -> [a]
justs = foldr cons []
