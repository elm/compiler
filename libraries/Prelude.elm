
module Prelude where

import Native.Prelude as N

radians : Float -> Float
radians t = t

degrees : Float -> Float
degrees d = d * Math.PI / 180

turns : Float -> Float
turns r = 2 * Math.PI * r

(+) : Number -> Number -> Number
(-) : Number -> Number -> Number
(*) : Number -> Number -> Number
(/) : Float -> Float -> Float

-- Integer division, remainder is discarded.
div : Int -> Int -> Int

-- Finds the remainder after dividing one number by another: 4 `rem` 3 == 1
rem : Int -> Int -> Int

-- Perform modular arithmetic: 7 `mod` 2 == 1
mod : Int -> Int -> Int

-- Basic functions of trigonometry.
cos  : Float -> Float
sin  : Float -> Float
tan  : Float -> Float
acos : Float -> Float
asin : Float -> Float
atan : Float -> Float

-- Returns the arctangent of the quotient of its arguments in radians.
-- So `atan2 y x` computes the angle from the positive x-axis to the vector
-- starting at the origin and ending at (x,y).
atan2 : Float -> Float -> Float

-- Take the square root of a number.
sqrt : Number -> Number

-- Take the absolute value of a number.
abs : Number -> Number

-- Calculate the logarithm of a number with a given base: `logBase 10 100 == 2`
logBase : Number -> Number -> Number

-- Given two numbers, returns the smaller (or greater respectively).
min : Number -> Number -> Number
max : Number -> Number -> Number

-- Clamps a number within a given range, so `clamp 100 200 x` is 200 for
-- x >= 200, 100 for x <= 100, and x for any 100 < x < 200 
clamp : Number -> Number -> Number -> Number

-- An approximation of pi.
pi : Float

-- An approximation of e.
e : Float

-- Compare any two values for structural equality and inequality.
-- Functions cannot be compared.
(==) : a -> a -> Bool
(/=) : a -> a -> Bool

-- Compare any two comparable values. Comparable values include String, Char,
-- Int, Float, Time, or a list or tuple containing comparable values.
-- These are also the only values that work as Dictionary keys or Set members.
(<)  : a -> a -> Bool
(>)  : a -> a -> Bool
(<=) : a -> a -> Bool
(>=) : a -> a -> Bool

-- The and operator. True if both inputs are True.
(&&) : Bool -> Bool -> Bool

-- The or operator. True if one or both inputs are True.
(||) : Bool -> Bool -> Bool

-- The exclusive-or operator. True if exactly one input is True.
xor : Bool -> Bool -> Bool

-- Negate a boolean value: (not True == False) and (not False == True)
not : Bool -> Bool

-- Equal to true. Useful as the last case of a multi-way-if.
otherwise : Bool
otherwise = True


-- Conversions

-- Round a number to the nearest integer.
round : Float -> Int

-- Truncate a decimal number, rounding towards zero.
truncate : Float -> Int

-- Floor function, rounding down.
floor : Float -> Int

-- Ceiling function, rounding up.
ceiling : Float -> Int

-- Convert an integer into a float.
toFloat : Int -> Float

-- Convert almost any value to its string representation.
show : a -> String

-- Read an integer from a string
readInt : String -> Maybe Int

-- Read a float from a string.
readFloat : String -> Maybe Float

-- Function Helpers

-- Function composition: f . g == (\x -> f (g x))
(.) : (b -> c) -> (a -> b) -> (a -> c)

-- Function application `f $ x == f x`. This function is useful for avoiding
-- parenthesis. Consider the following code to create a text element:
--     text (monospace (toText "code"))
-- This can also be written as:
--     text . monospace $ toText "code"
($) : (a -> b) -> a -> b

-- Given a value, returns exactly the same value.
id : a -> a

-- Given a 2-tuple, returns the first value.
fst : (a,b) -> a

-- Given a 2-tuple, returns the second value.
snd : (a,b) -> b

-- Flips the order of the first two arguments to a function.
flip : (a -> b -> c) -> (b -> a -> c)

-- Change how arguments are passed to a function. This splits paired arguments
-- into two separate arguments.
curry : ((a,b) -> c) -> a -> b -> c

-- Change how arguments are passed to a function. This combines two arguments
-- into a sigle pair.
uncurry : (a -> b -> c) -> (a,b) -> c
