
any f l =
    case l of
        x :: xs ->
            if f x then
                True
            else
                any f xs
        _ ->
            False