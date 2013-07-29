-- This library is a way to package up dynamic behavior. It makes it easier to
-- dynamically create dynamic components. See the [original release
-- notes](/blog/announce/version-0.5.0.elm) on this library to get a feel for how
-- it can be used.
module Automaton where

import open Basics
import Signal (lift,foldp,Signal)
import open List
import Maybe (Just, Nothing)

data Automaton a b = Step (a -> (Automaton a b, b))

-- Run an automaton on a given signal. The automaton steps forward
-- whenever the input signal updates.
run : Automaton a b -> b -> Signal a -> Signal b
run auto base inputs =
  let step a (Step f, _) = f a
  in  lift (\(x,y) -> y) (foldp step (auto,base) inputs)

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
  Step (\a -> let (autos', bs) = unzip (map (step a) autos)
              in  (combine autos', bs))

-- Create an automaton with no memory. It just applies the given function to
-- every input.
pure : (a -> b) -> Automaton a b
pure f = Step (\x -> (pure f, f x))

-- Create an automaton with state. Requires an initial state and a step
-- function to step the state forward. For example, an automaton that counted
-- how many steps it has taken would look like this:
--
--         count = Automaton a Int
--         count = state 0 (\\_ c -> c+1)
--
-- It is a stateful automaton. The initial state is zero, and the step function
-- increments the state on every step.
state : b -> (a -> b -> b) -> Automaton a b
state s f = Step (\x -> let s' = f x s
                        in  (state s' f, s'))

-- Create an automaton with hidden state. Requires an initial state and a
-- step function to step the state forward and produce an output.
hiddenState : s -> (a -> s -> (s,b)) -> Automaton a b
hiddenState s f = Step (\x -> let (s',out) = f x s
                              in  (hiddenState s' f, out))

-- Count the number of steps taken.
count : Automaton a Int
count = state 0 (\_ c -> c + 1)

type Queue t = ([t],[t])
empty = ([],[])
enqueue x (en,de) = (x::en, de)
dequeue q = case q of
              ([],[]) -> Nothing
              (en,[]) -> dequeue ([], reverse en)
              (en,hd::tl) -> Just (hd, (en,tl))

-- Computes the running average of the last `n` inputs.
average : Int -> Automaton Float Float
average k =
  let step n (ns,len,sum) =
          if len == k then stepFull n (ns,len,sum)
                      else ((enqueue n ns, len+1, sum+n), (sum+n) / (len+1))
      stepFull n (ns,len,sum) =
          case dequeue ns of
            Nothing -> ((ns,len,sum), 0)
            Just (m,ns') -> let sum' = sum + n - m
                            in ((enqueue n ns', len, sum'), sum' / len)
  in  hiddenState (empty,0,0) step


{-- TODO(evancz): See the following papers for ideas on how to make this
library faster and better:

- Functional Reactive Programming, Continued
- Causal commutative arrows and their optimization

Speeding things up is a really low priority. Language features and
libraries with nice APIs and are way more important!
--}

