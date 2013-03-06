
data Either a b = Left a | Right b

either : (a -> c) -> (b -> c) -> Either a b -> c
either f g e = case e of { Left x -> f x ; Right y -> g y }


isLeft  : Either a b -> Bool
isLeft  e = case e of { Left  _ -> True ; _ -> False }

isRight : Either a b -> Bool
isRight e = case e of { Right _ -> True ; _ -> False }


lefts  : [Either a b] -> [a]
lefts  = Elm.List.filter isLeft

rights : [Either a b] -> [b]
rights = Elm.List.filter isRight

partition : [Either a b] -> ([a],[b])
partition = Elm.List.partition isLeft