
module Automaton where

data Automaton inp out = Automaton (inp -> (out, Automaton inp out))

{--- API possibilies

All names and types are up for debate!


How to run an automaton (some require typeclasses):

run :: Automaton a b -> Signal a -> Signal b
run :: OrderedContainer c => Automaton a b -> c a -> c b
run :: Functor f => Automaton a b -> f a -> f b


compose :: Automaton a b -> Automaton b c -> Automaton a c
combine :: [Automaton a b] -> Automaton a [b]

init  :: b -> (a -> b -> b) -> Automaton a b
init' :: s -> (a -> s -> (b,s)) -> Automaton a b

--}