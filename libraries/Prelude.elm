
module Prelude where

import Native.Prelude as N

-- Convert radians to standard Elm angles (radians).
radians : Float -> Float
radians t = t

-- Convert degrees to standard Elm angles (radians).
degrees : Float -> Float
degrees d = d * Math.PI / 180

-- Convert turns to standard Elm angles (radians).
-- One turn is equal to 360&deg;.
turns : Float -> Float
turns r = 2 * Math.PI * r

-- Start with polar coordinates (r,&theta;)
-- and get out cartesian coordinates (x,y).
fromPolar : (Float,Float) -> (Float,Float)
fromPolar (r,t) = (r * N.cos t, r * N.sin t)

-- Start with cartesian coordinates (x,y)
-- and get out polar coordinates (r,&theta;).
toPolar : (Float,Float) -> (Float,Float)
toPolar (x,y) = (N.sqrt (x^2 + y^2), N.atan2 y x)

(+) : Number a -> Number a -> Number a
(-) : Number a -> Number a -> Number a
(*) : Number a -> Number a -> Number a

-- Floating point division.
(/) : Float -> Float -> Float

-- Integer division, remainder is discarded.
div : Int -> Int -> Int

-- Finds the remainder after dividing one number by another: 4 `rem` 3 == 1
rem : Int -> Int -> Int

-- Perform modular arithmetic: 7 `mod` 2 == 1
mod : Int -> Int -> Int

-- Exponentiation: `3^2 == 9`
(^) : Number a -> Number a -> Number a

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
sqrt : Number a -> Number a

-- Take the absolute value of a number.
abs : Number a -> Number a

-- Calculate the logarithm of a number with a given base: `logBase 10 100 == 2`
logBase : Number a -> Number a -> Number a

-- Given two numbers, returns the smaller (or greater respectively).
min : Number a -> Number a -> Number a
max : Number a -> Number a -> Number a

-- Clamps a number within a given range, so `clamp 100 200 x` is 200 for
-- x >= 200, 100 for x <= 100, and x for any 100 < x < 200 
clamp : Number a -> Number a -> Number a -> Number a

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
(<)  : Comparable a -> Comparable a -> Bool
(>)  : Comparable a -> Comparable a -> Bool
(<=) : Comparable a -> Comparable a -> Bool
(>=) : Comparable a -> Comparable a -> Bool

compare : Comparable a -> Comparable a -> Order

data Order = LT | EQ | GT

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

-- Forward function application `x |> f == f x`. This function is useful
-- for avoiding parenthesis and writing code in a more natural way.
-- Consider the following code to create a pentagon:
--
--     scale 2 (move 100 100 (filled blue (ngon 5 30)))
--
-- This can also be written as:
--
--     ngon 5 30 |> filled blue
--               |> move 100 100
--               |> scale 2
(|>) : a -> (a -> b) -> b

-- Function application `f <| x == f x`. This function is useful for avoiding
-- parenthesis. Consider the following code to create a text element:
--
--     text (monospace (toText "code"))
--
-- This can also be written as:
--
--     text . monospace <| toText "code"
(<|) : (a -> b) -> a -> b

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
