
import Debug

test x =
    case x of
        1 ->
            2
        _ ->
            Debug.crash "unexpected value"