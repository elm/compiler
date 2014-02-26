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

{-| Provide a default value and a function to extract the contents of a `Maybe`.
When given `Nothing` you get the default, when given a `Just` you apply the
function to the associated value.

      isPositive : Maybe Int -> Bool
      isPositive maybeInt = maybe False (\n -> n > 0) maybeInt

      map : (a -> b) -> Maybe a -> Maybe b
      map f m = maybe Nothing (\x -> Just (f x)) m
-}
maybe : b -> (a -> b) -> Maybe a -> b
maybe b f m = case m of
                Just v  -> f v
                Nothing -> b

{-| Check if a maybe happens to be a `Just`.

      isJust (Just 42) == True
      isJust (Just []) == True
      isJust Nothing   == False
-}
isJust : Maybe a -> Bool
isJust = maybe False (\_ -> True)

{-| Check if constructed with `Nothing`.

      isNothing (Just 42) == False
      isNothing (Just []) == False
      isNothing Nothing   == True
-}
isNothing : Maybe a -> Bool
isNothing = not . isJust

cons : Maybe a -> [a] -> [a]
cons mx xs = maybe xs (\x -> x :: xs) mx

{-| Filters out Nothings and extracts the remaining values.

      justs [Just 0, Nothing, Just 5, Just 7] == [0,5,7]
-}
justs : [Maybe a] -> [a]
justs = foldr cons []
