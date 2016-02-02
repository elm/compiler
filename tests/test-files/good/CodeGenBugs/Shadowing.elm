-- From: https://github.com/elm-lang/elm-compiler/issues/1086
--
-- Problem is that the `a` in (Second a b) shadowed the argument to `toName`,
-- actually overwriting the value.


type AType
    = First
    | Second String String


toName : AType -> String
toName a =
  let name =
        case a of
          First ->
              "True"

          Second a b ->
              a
  in
    name
