
module Automaton where

data Automaton a b = Automaton (a -> (b, Automaton a b))


{--- API possibilies: All names and types are up for debate!

run :: Automaton a b -> Signal a -> Signal b
run :: OrderedContainer c => Automaton a b -> c a -> c b

(>>>) :: Automaton a b -> Automaton b c -> Automaton a c
(<<<) :: Automaton b c -> Automaton a b -> Automaton a c

combine :: [Automaton a b] -> Automaton a [b]
combine :: Container c => c (Automaton a b) -> Automaton a (c b)


pure  :: (a -> b) -> Automaton a b
init  :: b -> (a -> b -> b) -> Automaton a b
init' :: s -> (a -> s -> (b,s)) -> Automaton a b

count :: Automaton a Int

draggable :: Form -> Automaton (Bool,(Int,Int)) Form

(^>>) :: (a -> b) -> Automaton b c -> Automaton a c
(>>^) :: Automaton a b -> (b -> c) -> Automaton a c

(^<<) :: (b -> c) -> Automaton a b -> Automaton a c
(<<^) :: Automaton b c -> (a -> b) -> Automaton a c

--}


run (Automaton m0) input =
  lift fst $ foldp' (\a (b, Automaton m) -> m a) m0 input

a1 >>> a2 =
  let { Automaton m1 = a1 ; Automaton m2 = a2 } in
  Automaton 
    (\a -> let (b,m1') = m1 a in
           let (c,m2') = m2 b in (c, Automaton m1' >>> Automaton m2'))

a2 <<< a1 = a1 >>> a2
f  ^>> a  = pure f >>> a
a  >>^ f  = a >>> pure f
f  ^<< a  = a >>> pure f
a  <<^ f  = pure f >>> a

combine autos =
  Automaton (\a -> let (bs,autos') = unzip $ map (\m -> m a) autos in
                   (bs, combine autos'))

pure  f      = Automaton (\x -> (f x, pure f))
init  s step = Automaton (\a -> let s'     = step a s in (s', init s' step))
init' s step = Automaton (\a -> let (b,s') = step a s in (b , init s' step))

count = init 0 (\_ c -> c + 1)



{--- See the following papers for ideas on how to make this faster:

- Functional Reactive Programming, Continued
- Causal commutative arrows and their optimization

Speeding things up is a really low priority. Language features and
libraries with nice APIs and are way more important!

--}