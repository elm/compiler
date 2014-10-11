
module Basics where

{-| Tons of useful functions that get imported by default.

The following libraries also get imported by default: List, Signal,
Text, Maybe, Time, Graphics.Element, Color, Graphics.Collage.

# Equality
@docs (==), (/=)

# Comparison

These functions only work on `comparable` types. This includes numbers,
characters, strings, lists of comparable things, and tuples of comparable
things. Note that tuples with 7 or more elements are not comparable; why
are your tuples so big?

@docs (<), (>), (<=), (>=), max, min, Order, compare

# Booleans
@docs not, (&&), (||), xor, otherwise

# Mathematics
@docs (+), (-), (*), (/), (^), (//), rem, (%), negate, abs, sqrt, clamp, logBase, e

# Trigonometry
@docs pi, cos, sin, tan, acos, asin, atan, atan2

# Number Conversions
@docs round, floor, ceiling, truncate, toFloat

# Angle Conversions
All angle conversions result in &ldquo;standard Elm angles&rdquo;
which happen to be radians.

@docs degrees, radians, turns

# Polar Coordinates
@docs toPolar, fromPolar

# Floating Point Checks
@docs isNaN, isInfinite

# Tuples
@docs fst, snd

# Higher-Order Helpers
@docs identity, always, (<|), (|>), (<<), (>>), flip, curry, uncurry

-}

import Native.Basics

{-| Convert radians to standard Elm angles (radians). -}
radians : Float -> Float
radians t = t

{-| Convert degrees to standard Elm angles (radians). -}
degrees : Float -> Float
degrees = Native.Basics.degrees

{-| Convert turns to standard Elm angles (radians).
One turn is equal to 360&deg;.
-}
turns : Float -> Float
turns = Native.Basics.turns

{-| Convert polar coordinates (r,&theta;) to cartesian coordinates (x,y). -}
fromPolar : (Float,Float) -> (Float,Float)
fromPolar = Native.Basics.fromPolar

{-| Convert cartesian coordinates (x,y) to polar coordinates (r,&theta;). -}
toPolar : (Float,Float) -> (Float,Float)
toPolar = Native.Basics.toPolar

(+) : number -> number -> number
(+) = Native.Basics.add

(-) : number -> number -> number
(-) = Native.Basics.sub

(*) : number -> number -> number
(*) = Native.Basics.mul

{-| Floating point division. -}
(/) : Float -> Float -> Float
(/) = Native.Basics.floatDiv

infixl 6 +
infixl 6 -
infixl 7 *
infixl 7 /
infixl 8 ^

infixl 7 //
infixl 7 %
infixl 7 `rem`

{-| Integer division. The remainder is discarded. -}
(//) : Int -> Int -> Int
(//) = Native.Basics.div

{-| Find the remainder after dividing one number by another.

       7 `rem` 2 == 1
      -1 `rem` 4 == -1
-}
rem : Int -> Int -> Int
rem = Native.Basics.rem

{-| Perform [modular arithmetic](http://en.wikipedia.org/wiki/Modular_arithmetic).

       7 % 2 == 1
      -1 % 4 == 3
-}
(%) : Int -> Int -> Int
(%) = Native.Basics.mod

{-| Exponentiation

      3^2 == 9`
-}
(^) : number -> number -> number
(^) = Native.Basics.exp

cos : Float -> Float
cos = Native.Basics.cos

sin : Float -> Float
sin = Native.Basics.sin

tan : Float -> Float
tan = Native.Basics.tan

acos : Float -> Float
acos = Native.Basics.acos

asin : Float -> Float
asin = Native.Basics.asin

{-| You probably do not want to use this. It takes `(y/x)` as the
argument, so there is no way to know whether the negative signs comes from
the `y` or `x`. Thus, the resulting angle is always between &pi;/2 and -&pi;/2
(in quadrants I and IV). You probably want to use `atan2` instead.
-}
atan : Float -> Float
atan = Native.Basics.atan

{-| This helps you find the angle of a cartesian coordinate.
You will almost certainly want to use this instead of `atan`.
So `atan2 y x` computes *atan(y/x)* but also keeps track of which
quadrant the angle should really be in. The result will be between
&pi; and -&pi;, giving you the full range of angles.
-}
atan2 : Float -> Float -> Float
atan2 = Native.Basics.atan2

{-| Take the square root of a number. -}
sqrt : Float -> Float
sqrt = Native.Basics.sqrt

{-| Negate a number.

      negate 42 == -42
      negate -42 == 42
      negate 0 == 0
-}
negate : number -> number
negate = Native.Basics.negate

{-| Take the absolute value of a number. -}
abs : number -> number
abs = Native.Basics.abs

{-| Calculate the logarithm of a number with a given base.

      logBase 10 100 == 2
      logBase 2 256 == 8
-}
logBase : Float -> Float -> Float
logBase = Native.Basics.logBase

{-| Clamps a number within a given range. With the expression
`clamp 100 200 x` the results are as follows:

      100     if x < 100
       x      if 100 <= x < 200
      200     if 200 <= x
-}
clamp : number -> number -> number -> number
clamp = Native.Basics.clamp

{-| An approximation of pi. -}
pi : Float
pi = Native.Basics.pi

{-| An approximation of e. -}
e : Float
e = Native.Basics.e

(==) : a -> a -> Bool
(==) = Native.Basics.eq

(/=) : a -> a -> Bool
(/=) = Native.Basics.neq

(<)  : comparable -> comparable -> Bool
(<)  = Native.Basics.lt

(>)  : comparable -> comparable -> Bool
(>)  = Native.Basics.gt

(<=) : comparable -> comparable -> Bool
(<=) = Native.Basics.le

(>=) : comparable -> comparable -> Bool
(>=) = Native.Basics.ge

infix 4 ==
infix 4 /=
infix 4 <
infix 4 >
infix 4 <=
infix 4 >=

{-| Compare any two comparable values. Comparable values include `String`, `Char`,
`Int`, `Float`, `Time`, or a list or tuple containing comparable values.
These are also the only values that work as `Dict` keys or `Set` members.
-}
compare : comparable -> comparable -> Order
compare = Native.Basics.compare

{-| Represents the relative ordering of two things.
The relations are less than, equal to, and greater than.
-}
data Order = LT | EQ | GT

{-| Find the smaller of two comparables. -}
min : comparable -> comparable -> comparable
min = Native.Basics.min

{-| Find the larger of two comparables. -}
max : comparable -> comparable -> comparable
max = Native.Basics.max

{-| The logical AND operator. `True` if both inputs are `True`.
This operator short-circuits to `False` if the first argument is `False`.
-}
(&&) : Bool -> Bool -> Bool
(&&) = Native.Basics.and

{-| The logical OR operator. `True` if one or both inputs are `True`.
This operator short-circuits to `True` if the first argument is True.
-}
(||) : Bool -> Bool -> Bool
(||) = Native.Basics.or

infixr 3 &&
infixr 2 ||

{-| The exclusive-or operator. `True` if exactly one input is `True`. -}
xor : Bool -> Bool -> Bool
xor = Native.Basics.xor

{-| Negate a boolean value.

    not True == False
    not False == True
-}
not : Bool -> Bool
not = Native.Basics.not

{-| Equal to `True`. Useful as the last case of a multi-way-if. -}
otherwise : Bool
otherwise = True


-- Conversions

{-| Round a number to the nearest integer. -}
round : Float -> Int
round = Native.Basics.round

{-| Truncate a number, rounding towards zero. -}
truncate : Float -> Int
truncate = Native.Basics.truncate

{-| Floor function, rounding down. -}
floor : Float -> Int
floor = Native.Basics.floor

{-| Ceiling function, rounding up. -}
ceiling : Float -> Int
ceiling = Native.Basics.ceiling

{-| Convert an integer into a float. -}
toFloat : Int -> Float
toFloat = Native.Basics.toFloat

{- | Determine whether a float is an undefined or unrepresentable number.
NaN stands for *not a number* and it is [a standardized part of floating point
numbers](http://en.wikipedia.org/wiki/NaN).

      isNaN (0/0)     == True
      isNaN (sqrt -1) == True
      isNaN (1/0)     == False  -- infinity is a number
      isNaN 1         == False
-}
isNaN : Float -> Bool
isNaN = Native.Basics.isNaN

{- | Determine whether a float is positive or negative infinity.

      isInfinite (0/0)     == False
      isInfinite (sqrt -1) == False
      isInfinite (1/0)     == True
      isInfinite 1         == False

Notice that NaN is not infinite! For float `n` to be finite implies that
`not (isInfinite n || isNaN n)` evaluates to `True`.
-}
isInfinite : Float -> Bool
isInfinite = Native.Basics.isInfinite

-- Function Helpers

{-| Function composition, passing results along in the suggested direction. For
example, the following code checks if the square root of a number is odd:

      not << isEven << sqrt

You can think of this operator as equivalent to the following:

      (g << f)  ==  (\x -> g (f x))

So our example expands out to something like this:

      \n -> not (isEven (sqrt n))
-}
(<<) : (b -> c) -> (a -> b) -> (a -> c)
(<<) g f x = g (f x)

{-| Function composition, passing results along in the suggested direction. For
example, the following code checks if the square root of a number is odd:

      sqrt >> isEven >> not

This direction of function composition seems less pleasant than `(<<)` which
reads nicely in expressions like: `filter (not << isRegistered) students`
-}
(>>) : (a -> b) -> (b -> c) -> (a -> c)
(>>) f g x = g (f x)

{-| Forward function application `x |> f == f x`. This function is useful
for avoiding parenthesis and writing code in a more natural way.
Consider the following code to create a pentagon:

        scale 2 (move (10,10) (filled blue (ngon 5 30)))

This can also be written as:

        ngon 5 30 |> filled blue
                  |> move (10,10)
                  |> scale 2
-}
(|>) : a -> (a -> b) -> b
x |> f = f x

{-| Backward function application `f <| x == f x`. This function is useful for
avoiding parenthesis. Consider the following code to create a text element:

        text (monospace (toText "code"))

This can also be written as:

        text . monospace <| toText "code"
-}
(<|) : (a -> b) -> a -> b
f <| x = f x

infixr 9 <<
infixl 9 >>
infixr 0 <|
infixl 0 |>

{-| Given a value, returns exactly the same value. This is called
[the identity function](http://en.wikipedia.org/wiki/Identity_function).
-}
identity : a -> a
identity x = x

{-| Create a [constant function](http://en.wikipedia.org/wiki/Constant_function),
a function that *always* returns the same value regardless of what input you give.
It is defined as:

        always a b = a

It totally ignores the second argument, so `always 42` is a function that always
returns 42. When you are dealing with higher-order functions, this comes in
handy more often than you might expect. For example, creating a zeroed out list
of length ten would be:

        map (always 0) [0..9]
-}
always : a -> b -> a
always a _ = a

{-| Given a 2-tuple, returns the first value. -}
fst : (a,b) -> a
fst (a,_) = a

{-| Given a 2-tuple, returns the second value. -}
snd : (a,b) -> b
snd (_,b) = b

{-| Flip the order of the first two arguments to a function. -}
flip : (a -> b -> c) -> (b -> a -> c)
flip f b a = f a b

{-| Change how arguments are passed to a function.
This splits paired arguments into two separate arguments.
-}
curry : ((a,b) -> c) -> a -> b -> c
curry f a b = f (a,b)

{-| Change how arguments are passed to a function.
This combines two arguments into a single pair.
-}
uncurry : (a -> b -> c) -> (a,b) -> c
uncurry f (a,b) = f a b
