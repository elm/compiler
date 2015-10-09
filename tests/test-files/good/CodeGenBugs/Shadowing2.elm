-- https://github.com/elm-lang/elm-compiler/issues/1090
--
-- The problem was shadowing a variable used in a pattern match, but then
-- rebinding the variable before using all the subparts. There was an issue
-- in which inlining of certain values let names jump into scopes where they
-- did not belong.


type Maybe a
    = Just a
    | Nothing


withDefault0 : Maybe Int -> Int
withDefault0 maybe =
  case maybe of
    Just int ->
      let
        maybe =
          Just "shadow argument, but do not overwrite it in JS!"
      in
        int -- should be from the `maybe` in the argument, not the shadow

    _ ->
      0
