
module Automaton where

data Automaton a b = Automaton (a -> (b, Automaton a b))

-- Maybe once there are infix type constructors this could be
--     data a ~> b
-- or something like that.


{--- API possibilies

All names and types are up for debate!


How to run an automaton (some require typeclasses):

run :: Automaton a b -> Signal a -> Signal b
run :: OrderedContainer c => Automaton a b -> c a -> c b

compose :: Automaton a b -> Automaton b c -> Automaton a c

combine :: [Automaton a b] -> Automaton a [b]
combine :: Container c => c (Automaton a b) -> Automaton a (c b)

init  :: b -> (a -> b -> b) -> Automaton a b
init' :: s -> (a -> s -> (b,s)) -> Automaton a b

draggable :: Form -> Automaton (Bool,(Int,Int)) Form

--}


run (Automaton m0) input =
    lift fst $ foldp1 (\a (b, Automaton m) -> m a) m0 input

compose (Automaton m1) (Automaton m2) =
    Automaton 
    (\a -> let (b,m1') = m1 a in
           let (c,m2') = m2 b in (c, compose (Automaton m1') (Automaton m2')))

combine autos =
    Automaton (\a -> let (bs,autos') = unzip $ map (\m -> m a) autos in
                     (bs, combine autos'))

init s step = 
  Automaton (\a -> let s' = step a s in (s', init s' step))

init' s step =
  Automaton (\a -> let (b,s') = step a s in (b, init s' step))
