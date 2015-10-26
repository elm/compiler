
import Debug

test x =
    if x then
        2
    else
        Debug.crash "unexpected value"