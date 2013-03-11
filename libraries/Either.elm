
module Either where

import List

data Either a b = Left a | Right b

either : (a -> c) -> (b -> c) -> Either a b -> c
either f g e = case e of { Left x -> f x ; Right y -> g y }


isLeft : Either a b -> Bool
isLeft e = case e of { Left  _ -> True ; _ -> False }

isRight : Either a b -> Bool
isRight e = case e of { Right _ -> True ; _ -> False }


--lefts : [Either a b] -> [a]
lefts es = List.filter isLeft es

--rights : [Either a b] -> [b]
rights es = List.filter isRight es

--partition : [Either a b] -> ([a],[b])
partition es = List.partition isLeft es