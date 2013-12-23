module Maybe where

{-| Represents an optional value. Maybe it is there, maybe it is not.

# Type and Constructors
@docs Maybe, justIf

# Examining Maybes
@docs isJust, isNothing

# Transforming Maybes
@docs map, maybe

# Extracting values
@docs extract, justs, keepJusts
-}

import Basics (not, otherwise, (.))
import List (foldr)
import Signal (Signal, lift, keepIf)

{-| The `Maybe` datatype. Useful when a computation may or may not
result in a value (e.g. logarithm is defined only for positive numbers).
-}
data Maybe a = Just a | Nothing

{-| Return `Just` the value if it meets the predicate, and `Nothing` otherwise.
-}
justIf : (a -> Bool) -> a -> Maybe a
justIf p a = if p a then Just a else Nothing

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

{-| Return the value in a `Just`, or the default if given `Nothing`.
-}
extract : a -> Maybe a -> a
extract a m = case m of
                Just v -> v
                Nothing -> a

{-| Create a signal using the value of each `Just`, dropping the `Nothings`. A
default is required and used until the input signal is not `Nothing`.
-}
keepJusts : a -> Signal (Maybe a) -> Signal a
keepJusts a sa = lift (\(Just x) -> x) (keepIf isJust (Just a) sa)
