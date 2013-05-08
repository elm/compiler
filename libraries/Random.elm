
module Random where

import Native.Random as R

-- Given a range from low to high and a signal of values, this produces
-- a new signal that changes whenever the input signal changes. The new
-- values are random number between 'low' and 'high' inclusive.
range : Int -> Int -> Signal a -> Signal Int

-- Produces a new signal that changes whenever the input signal changes.
-- The new values are random number in [0..1).
float : Signal a -> Signal Float
