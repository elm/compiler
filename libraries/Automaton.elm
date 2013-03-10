
module Automaton where

data Automaton a b = Automaton (a -> (b, Automaton a b))

run : Automaton a b -> Signal a -> Signal b
run (Automaton m0) input =
  lift fst $ foldp' (\a (b, Automaton m) -> m a) m0 input

step : Automaton a b -> a -> (b, Automaton a b)
step (Automaton m) a = m a


(>>>) : Automaton a b -> Automaton b c -> Automaton a c
a1 >>> a2 =
  let Automaton m1 = a1
      Automaton m2 = a2
  in  Automaton (\a -> let (b,m1') = m1 a
                           (c,m2') = m2 b
                       in  (c, m1' >>> m2'))

(<<<) : Automaton b c -> Automaton a b -> Automaton a c
a2 <<< a1 = a1 >>> a2 

combine : [Automaton a b] -> Automaton a [b]
combine autos =
  Automaton (\a -> let (bs,autos') = unzip $ map (\(Automaton m) -> m a) autos in
                   (bs, combine autos'))

pure  : (a -> b) -> Automaton a b
pure  f      = Automaton (\x -> (f x, pure f))

init  : b -> (a -> b -> b) -> Automaton a b
init  s step = Automaton (\a -> let s'     = step a s in (s', init  s' step))

init' : s -> (a -> s -> (b,s)) -> Automaton a b
init' s step = Automaton (\a -> let (b,s') = step a s in (b , init' s' step))

count : Automaton a Int
count = init 0 (\_ c -> c + 1)


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

draggable : Form -> Automaton (Bool,(Int,Int)) Form
draggable form = init' (Listen,form) stepDrag


{--- See the following papers for ideas on how to make this faster:

- Functional Reactive Programming, Continued
- Causal commutative arrows and their optimization

Speeding things up is a really low priority. Language features and
libraries with nice APIs and are way more important!

--}