-- https://github.com/elm-lang/elm-compiler/issues/1223
--
-- The problem: shadowing a variable through let.


type Maybe a
    = Just a
    | Nothing


bad arg =
  (\x -> x)
    (case arg of
      Just _ ->
        True

      Nothing ->
        let
          arg = True
        in
          if arg then False else arg)

