module Random where

{-| Since the core of Elm is pure, randomness must be handled via signals.

# Random Numbers
@docs range, float
-}

import Signal (Signal)
import Native.Random

{-| Given a range from low to high and a signal of values, this produces
a new signal that changes whenever the input signal changes. The new
values are random number between 'low' and 'high' inclusive.
-}
range : Int -> Int -> Signal a -> Signal Int
range = Native.Random.range

{-| Produces a new signal that changes whenever the input signal changes.
The new values are random numbers in [0..1).
-}
float : Signal a -> Signal Float
float = Native.Random.flt
