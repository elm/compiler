
{- From: https://github.com/elm-lang/elm-compiler/issues/1086 -}

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
