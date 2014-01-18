import open Either
import open Trampoline

f n =
 if | n == 0 -> Right 0
    | otherwise -> Left <| n - 1

data MapResult a b
 = Done [b]
 | Continue [a] [b]

tmap: (a -> b) -> [a] -> [b]
tmap f xs =
 trampoline (tmap' f xs [] 0)
            (\ thunk ->
                case thunk of
                 Continue input result -> Left <| tmap' f input result 0
                 Done result -> Right result)

tmap': (a -> b) -> [a] -> [b] -> Int -> MapResult a b
tmap' f xs r n =
 if | n == 100 -> Continue xs r
    | otherwise ->
     case xs of
      (x::xs) -> tmap' f xs (f x::r) (n + 1)
      [] -> Done <| reverse r

naiveMap: (a -> b) -> [a] -> [b]
naiveMap f xs =
 case xs of
  [] -> []
  (x::xs) -> f x :: naiveMap f xs

main =
 flow down
  [asText <| trampoline 300000 f
  ,asText <| head <| tmap (\n->n+1) [1..1000000]
  --,asText <| head <| naiveMap (\n->n+1) [1..1000000]
  ]