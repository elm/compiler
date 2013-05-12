
module Maybe where

import List as List

-- The Maybe datatype. Useful when a computation may or may not
-- result in a value (e.g. logarithm is defined only for positive numbers).
data Maybe a = Just a | Nothing

-- Apply a function to the contents of a `Maybe`.
-- Return default when given `Nothing`.
maybe : b -> (a -> b) -> Maybe a -> b
maybe b f m = case m of
                Just v  -> f v
                Nothing -> b

-- Check if constructed with `Just`.
isJust : Maybe a -> Bool
isJust = maybe False (\_ -> True)

-- Check if constructed with `Nothing`.
isNothing : Maybe a -> Bool
isNothing = not . isJust


-- If `Just`, adds the value to the front of the list.
-- If `Nothing`, list is unchanged.
cons : Maybe a -> [a] -> [a]
cons mx xs = maybe xs (\x -> x :: xs) mx

-- Filters out Nothings and extracts the remaining values.
justs : [Maybe a] -> [a]
justs = List.foldr cons []
