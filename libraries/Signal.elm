
-- The library for general signal manipulation. Some useful functions for
-- working with time (e.g. setting FPS) and combining signals and time (e.g.
-- delaying updates, getting timestamps) can be found in the
-- [`Time`](/docs/Signal/Time.elm) library.
--
-- Note: There are lift functions up to `lift8`.
module Signal where

import Native.Signal as S
import List as L

-- Create a constant signal that never changes.
constant : a -> Signal a

-- Transform a signal with a given function.
lift  : (a -> b) -> Signal a -> Signal b

-- Combine two signals with a given function.
lift2 : (a -> b -> c) -> Signal a -> Signal b -> Signal c

-- Combine three signals with a given function.
lift3 : (a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> Signal d
lift4 : (a -> b -> c -> d -> e) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
lift5 : (a -> b -> c -> d -> e -> f) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f
lift6 : (a -> b -> c -> d -> e -> f -> g) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f -> Signal g
lift7 : (a -> b -> c -> d -> e -> f -> g -> h) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f -> Signal g -> Signal h
lift8 : (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f -> Signal g -> Signal h -> Signal i

-- Create a past-dependent signal. Each value given on the input signal will
-- be accumulated, producing a new output value.
--
-- For instance, `(foldp (\\t acc -> acc + 1) 0 (Time.every second))` increments every second.
foldp : (a -> b -> b) -> b -> Signal a -> Signal b

-- Merge two signals into one, biased towards the first signal if both signals
-- update at the same time.
merge : Signal a -> Signal a -> Signal a

-- Merge many signals into one, biased towards the left-most signal if multiple
-- signals update simultaneously.
merges : [Signal a] -> Signal a

-- Combine many signals into one, with an array of results.
combine : [Signal a] -> Signal [a]
combine = L.foldr (S.lift2 (::)) (S.constant [])

-- Merge two signals into one, but distinguishing the values by marking the first
-- signal as `Left` and the second signal as `Right`. This allows you to easily
-- fold over non-homogeneous inputs.
mergeEither : Signal a -> Signal b -> Signal (Either a b)

-- Count the number of events that have occured.
count : Signal a -> Signal Int

-- Count the number of events that have occured that satisfy a given predicate.
countIf : (a -> Bool) -> Signal a -> Signal Int

-- Keep only events that satisfy the given predicate. Elm does not allow
-- undefined signals, so a base case must be provided in case the predicate is
-- never satisfied.
keepIf : (a -> Bool) -> a -> Signal a -> Signal a

-- Drop events that satisfy the given predicate. Elm does not allow undefined
-- signals, so a base case must be provided in case the predicate is never
-- satisfied.
dropIf : (a -> Bool) -> a -> Signal a -> Signal a

-- Keep events only when the first signal is true. When the first signal becomes
-- true, the most recent value of the second signal will be propagated. Until
-- the first signal becomes false again, all events will be propagated. Elm does
-- not allow undefined signals, so a base case must be provided in case the first
-- signal is never true.
keepWhen : Signal Bool -> a -> Signal a -> Signal a

-- Drop events when the first signal is true. When the first signal becomes false,
-- the most recent value of the second signal will be propagated. Until the first
-- signal becomes true again, all events will be propagated. Elm does not allow
-- undefined signals, so a base case must be provided in case the first signal is
-- always true.
dropWhen : Signal Bool -> a -> Signal a -> Signal a

-- Drop sequential repeated values. For example, if a signal produces the
-- sequence `[1,1,2,2,1]`, it becomes `[1,2,1]` by dropping the values that
-- are the same as the previous value.
dropRepeats : Signal a -> Signal a

-- Sample from the second input every time an event occurs on the first input.
-- For example, `(sampleOn clicks (every second))` will give the approximate
-- time of the latest click.
sampleOn : Signal a -> Signal b -> Signal b

-- Add a timestamp to any signal. Timestamps increase monotonically. Each timestamp is
-- related to a specfic event, so `Mouse.x` and `Mouse.y` will always have the same
-- timestamp because they both rely on the same underlying event.
timestamp : Signal a -> Signal (Time, a)

-- Delay a signal by a certain amount of time. So `(delay second Mouse.clicks)`
-- will update one second later than any mouse click.
delay : Time -> Signal a -> Signal a

-- An alias for `lift`. A prettier way to apply a
-- function to the current value of a signal.
(<~) : (a -> b) -> Signal a -> Signal b

-- Signal application. This takes two signals, holding a function and
-- a value. It applies the current function to the current value.
--
-- So the following expressions are equivalent:
--
--     scene <~ Mouse.x ~ Mouse.y
--     lift2 scene Mouse.x Mouse.y
(~) : Signal (a -> b) -> Signal a -> Signal b
