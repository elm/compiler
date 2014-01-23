module Maybe where

{-| Represents an optional value. Maybe it is there, maybe it is not.

# Type and Constructors
@docs Maybe

# Taking Maybes apart
@docs maybe, isJust, isNothing

# Maybes and Lists
@docs justs, headMaybe, tailMaybe, lastMaybe
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

{-| Extract the first element of a list.
`(headMaybe [1,2,3] == Just 1)`
-}
headMaybe : [a] -> Maybe a
headMaybe xs = case xs of
    []   -> Nothing
    x::_ -> Just x

{-| Extract the elements after the head of the list.
`(tailMaybe [1,2,3] == Just [2,3])`
-}
tailMaybe : [a] -> Maybe [a]
tailMaybe xs = case xs of
    []    -> Nothing
    _::xs -> Just xs
    
{-| Extract the last element of a list.
`(lastMaybe [1,2,3] == Just 3)`
-}
lastMaybe : [a] -> Maybe a
lastMaybe xs = case xs of
    []  -> Nothing
    _   -> Just (Native.List.last xs)
