
module Random where

import Native.Random

-- Given a range from low to high, this produces a random number
-- between 'low' and 'high' inclusive. The value in the signal does
-- not change after the page has loaded.
inRange : Int -> Int -> Signal Int
inRange = Native.Random.inRange

-- Given a range from low to high and a signal of values, this produces
-- a new signal that changes whenever the input signal changes. The new
-- values are random number between 'low' and 'high' inclusive.
randomize : Int -> Int -> Signal a -> Signal Int
randomize = Native.Random.randomize
