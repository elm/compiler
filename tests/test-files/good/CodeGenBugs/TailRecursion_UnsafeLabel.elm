-- From https://github.com/elm-lang/elm-compiler/issues/1104
--
-- When code can be tail call optimized, we use the name of the function as
-- the label. The label must be a safe name, not a raw thing. Otherwise it
-- could have primes in it!


type List a
    = Nil
    | Cons a (List a)


foldl' : (a -> b -> b) -> b -> List a -> b
foldl' update state items =
  case items of
    Nil ->
      state

    Cons item rest ->
      foldl' update (update item state) rest