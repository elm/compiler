
module Basics where

import Native.Basics as Native

-- Convert radians to standard Elm angles (radians).
radians : Float -> Float
radians t = t

-- Convert degrees to standard Elm angles (radians).
degrees : Float -> Float
degrees d = d * Native.pi / 180

-- Convert turns to standard Elm angles (radians).
-- One turn is equal to 360&deg;.
turns : Float -> Float
turns r = 2 * Native.pi * r

-- Start with polar coordinates (r,&theta;)
-- and get out cartesian coordinates (x,y).
fromPolar : (Float,Float) -> (Float,Float)
fromPolar (r,t) = (r * Native.cos t, r * Native.sin t)

-- Start with cartesian coordinates (x,y)
-- and get out polar coordinates (r,&theta;).
toPolar : (Float,Float) -> (Float,Float)
toPolar (x,y) = (Native.sqrt (x^2 + y^2), Native.atan2 y x)

(+) : number -> number -> number
(+) = Native.add

(-) : number -> number -> number
(-) = Native.sub

(*) : number -> number -> number
(*) = Native.mul

-- Floating point division.
(/) : Float -> Float -> Float
(/) = Native.floatDiv

-- Integer division, remainder is discarded.
div : Int -> Int -> Int
div = Native.div

-- Finds the remainder after dividing one number by another: ``4 `rem` 3 == 1``
rem : Int -> Int -> Int
rem = Native.rem

-- Perform modular arithmetic: ``7 `mod` 2 == 1``
mod : Int -> Int -> Int
mod = Native.mod

-- Exponentiation: `3^2 == 9`
(^) : number -> number -> number
(^) = Native.exp

cos  : Float -> Float
cos = Native.cos

sin  : Float -> Float
sin = Native.sin

tan  : Float -> Float
tan = Native.tan

acos : Float -> Float
acos = Native.acos

asin : Float -> Float
asin = Native.asin

-- You probably do not want to use this. Because it takes `(y/x)` as the argument
-- there is no way to know where the negative signs come from so the resulting
-- angle is always between &pi;/2 and -&pi;/2 (in quadrants I and IV).
atan : Float -> Float
atan = Native.atan

-- This helps you find the angle of a cartesian coordinate.
-- You will almost certainly want to use this instead of `atan`.
-- So `atan2 y x` computes *atan(y/x)* but also keeps track of which
-- quadrant the angle should really be in. The result will be between
-- &pi; and -&pi;, giving you the full range of angles.
atan2 : Float -> Float -> Float
atan2 = Native.atan2

-- Take the square root of a number.
sqrt : number -> number
sqrt = Native.sqrt

-- Take the absolute value of a number.
abs : number -> number
abs = Native.abs

-- Calculate the logarithm of a number with a given base: `logBase 10 100 == 2`
logBase : number -> number -> number
logBase = Native.logBase

-- Given two comparables, returns the smaller one.
min : comparable -> comparable -> comparable
min = Native.min

-- Given two comparables, returns the larger one.
max : comparable -> comparable -> comparable
max = Native.max

-- Clamps a number within a given range. With the expression `clamp 100 200 x`
-- the results are as follows:
--
--   * `100   if x < 100`
--   * ` x    if 100 <= x < 200`
--   * `200   if 200 <= x`
clamp : number -> number -> number -> number
clamp = Native.clamp

-- An approximation of pi.
pi : Float
pi = Native.pi

-- An approximation of e.
e : Float
e = Native.e

-- Compare any two values for structural equality. Functions cannot be compared.
(==) : a -> a -> Bool
(==) = Native.eq

(/=) : a -> a -> Bool
(/=) = Native.neq

(<)  : comparable -> comparable -> Bool
(<)  = Native.lt

(>)  : comparable -> comparable -> Bool
(>)  = Native.gt

(<=) : comparable -> comparable -> Bool
(<=) = Native.le

(>=) : comparable -> comparable -> Bool
(>=) = Native.ge

-- Compare any two comparable values. Comparable values include `String`, `Char`,
-- `Int`, `Float`, `Time`, or a list or tuple containing comparable values.
-- These are also the only values that work as `Dict` keys or `Set` members.
compare : comparable -> comparable -> Order
compare = Native.compare

-- Represents the relative ordering of two things.
-- The relations are less than, equal to, and greater than.
data Order = LT | EQ | GT

-- The and operator. True if both inputs are True.
(&&) : Bool -> Bool -> Bool
(&&) = Native.and

-- The or operator. True if one or both inputs are True.
(||) : Bool -> Bool -> Bool
(||) = Native.or

-- The exclusive-or operator. True if exactly one input is True.
xor : Bool -> Bool -> Bool
xor = Native.xor

-- Negate a boolean value: `(not True == False)` and `(not False == True)`
not : Bool -> Bool
not = Native.not

-- Equal to true. Useful as the last case of a multi-way-if.
otherwise : Bool
otherwise = True


-- Conversions

-- Round a number to the nearest integer.
round : Float -> Int
round = Native.round

-- Truncate a decimal number, rounding towards zero.
truncate : Float -> Int
truncate = Native.truncate

-- Floor function, rounding down.
floor : Float -> Int
floor = Native.floor

-- Ceiling function, rounding up.
ceiling : Float -> Int
ceiling = Native.ceiling

-- Convert an integer into a float.
toFloat : Int -> Float
toFloat = Native.toFloat

-- Convert almost any value to its string representation.
show : a -> String
show = Native.show

-- Read an integer from a string
readInt : String -> Maybe Int
readInt = Native.readInt

-- Read a float from a string.
readFloat : String -> Maybe Float
readFloat = Native.readFloat

-- Function Helpers

-- Function composition: `(f . g == (\\x -> f (g x)))`
(.) : (b -> c) -> (a -> b) -> (a -> c)
(.) f g x = f (g x)

-- Forward function application `x |> f == f x`. This function is useful
-- for avoiding parenthesis and writing code in a more natural way.
-- Consider the following code to create a pentagon:
--
--         scale 2 (move (10,10) (filled blue (ngon 5 30)))
--
-- This can also be written as:
--
--         ngon 5 30 |> filled blue
--                   |> move (10,10)
--                   |> scale 2
(|>) : a -> (a -> b) -> b
x |> f = f x

-- Function application `f <| x == f x`. This function is useful for avoiding
-- parenthesis. Consider the following code to create a text element:
--
--         text (monospace (toText "code"))
--
-- This can also be written as:
--
--         text . monospace <| toText "code"
(<|) : (a -> b) -> a -> b
f <| x = f x

-- Given a value, returns exactly the same value.
id : a -> a
id x = x

-- Given a 2-tuple, returns the first value.
fst : (a,b) -> a
fst = Native.fst

-- Given a 2-tuple, returns the second value.
snd : (a,b) -> b
snd = Native.snd

-- Flips the order of the first two arguments to a function.
flip : (a -> b -> c) -> (b -> a -> c)
flip f b a = f a b

-- Change how arguments are passed to a function. This splits paired arguments
-- into two separate arguments.
curry : ((a,b) -> c) -> a -> b -> c
curry = Native.curry

-- Change how arguments are passed to a function. This combines two arguments
-- into a sigle pair.
uncurry : (a -> b -> c) -> (a,b) -> c
uncurry = Native.uncurry