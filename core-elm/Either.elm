
data Either a b = Left a | Right b

either f g e = case e of { Left x -> f x ; Right y -> g y }

isLeft  e = case e of { Left  _ -> True ; _ -> False }
isRight e = case e of { Right _ -> True ; _ -> False }

lefts  = Elm.List.filter isLeft
rights = Elm.List.filter isRight

partition = Elm.List.partition isLeft