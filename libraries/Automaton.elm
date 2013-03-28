-- This library is a way to package up dynamic behavior. It makes it easier to
-- dynamically create dynamic components. See the [original release
-- notes](/blog/announce/version-0.5.0.elm) on this library to get a feel for how
-- it can be used.
module Automaton where

data Automaton a b = Step (a -> (Automaton a b, b))

-- Run an automaton on a given signal. The automaton steps forward
-- whenever the input signal updates.
run : Automaton a b -> b -> Signal a -> Signal b
run (Step f) base inputs =
  let step a (Step f, _) = f a
  in  lift snd $ foldp step base inputs

-- Step an automaton forward once with a given input.
step : a -> Automaton a b -> (Automaton a b, b)
step a (Step f) = f a

-- Compose two automatons, chaining them together.
(>>>) : Automaton a b -> Automaton b c -> Automaton a c
f >>> g =
  Step (\a -> let (f', b) = step a f
                  (g', c) = step b g
              in  (f' >>> g', c))

-- Compose two automatons, chaining them together.
(<<<) : Automaton b c -> Automaton a b -> Automaton a c
g <<< f = f >>> g

-- Combine a list of automatons into a single automaton that produces a list.
combine : [Automaton a b] -> Automaton a [b]
combine autos =
  Step (\a -> let (autos', bs) = unzip $ map (step a) autos
              in  (combine autos', bs))

-- Create an automaton with no memory. It just applies the given function to
-- every input.
pure : (a -> b) -> Automaton a b
pure f = Step (\x -> (pure f, f x))

-- Create an automaton with state. Requires an initial state and a step
-- function to step the state forward.
init : b -> (a -> b -> b) -> Automaton a b
init s f = Step (\x -> let s' = f x s
                       in  (init s' f, s'))

-- Create an automaton with hidden state. Requires an initial state and a
-- step function to step the state forward and produce an output.
init' : s -> (a -> s -> (s,b)) -> Automaton a b
init' s f = Step (\x -> let (s',out) = f x s
                        in  (init' s' f, out))

-- Count the number of steps taken.
count : Automaton a Int
count = init 0 (\_ c -> c + 1)


{-- TODO(evancz): move this code to the Form library so people can find it.

data DragState = Listen | Ignore | DragFrom (Int,Int)

vecSub (x1,y1) (x2,y2) = (x1-x2,y1-y2)

stepDrag (press,pos) (ds,form) =
  let wrap ds' = (form, (ds',form)) in
  case ds of
    Listen -> wrap (if | not press -> Listen
                       | pos `isWithin` form -> DragFrom pos
                       | otherwise -> Ignore)
    Ignore -> wrap (if press then Ignore else Listen)
    DragFrom p0 ->
        if press then (uncurry move (vecSub pos p0) form, (DragFrom p0, form))
                 else (let form' = uncurry move (vecSub pos p0) form in
                       (form', (Listen,form')))

-- Create a draggable form that can be dynamically created and added to a scene.
draggable : Form -> Automaton (Bool,(Int,Int)) Form
draggable form = init' (Listen,form) stepDrag
--}

{-- TODO(evancz): See the following papers for ideas on how to make this
library faster and better:

- Functional Reactive Programming, Continued
- Causal commutative arrows and their optimization

Speeding things up is a really low priority. Language features and
libraries with nice APIs and are way more important!
--}

