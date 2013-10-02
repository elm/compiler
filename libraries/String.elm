module String where
{-| A built-in representation of String for effecient string manipulation.
-}

import Native.String as Native
import Maybe (Maybe)

{-|
-}
isEmpty : String
isEmpty = Native.isEmpty

{-|
-}
cons : Char -> String -> String
cons = Native.cons

{-|
-}
uncons : String -> Maybe (Char, String)
uncons = Native.uncons

{-|
-}
map : (Char -> Char) -> String -> String
map = Native.map

{-|
-}
filter : (Char -> Bool) -> String -> String
filter = Native.filter

{-|
-}
reverse : String -> String
reverse = Native.reverse

{-|
-}
foldl : (Char -> b -> b) -> b -> String -> b
foldl = Native.foldl

{-|
-}
foldr : (Char -> b -> b) -> b -> String -> b
foldr = Native.foldr

{-|
-}
split : String -> String -> [String]
split = Native.split

{-|
-}
join : String -> [String] -> String
join = Native.join

{-|
-}
repeat : Int -> String -> String
repeat = Native.repeat

{-|
-}
center : Int -> Char -> String -> String
center = Native.center

{-|
-}
justifyLeft : Int -> Char -> String -> String
justifyLeft = Native.justifyLeft

{-|
-}
justifyRight : Int -> Char -> String -> String
justifyRight = Native.justifyRight

{-|
-}
trim : String -> String
trim = Native.trim

{-|
-}
trimLeft : String -> String
trimLeft = Native.trimLeft

{-|
-}
trimRight : String -> String
trimRight = Native.trimRight

{-|
-}
words : String -> [String]
words = Native.words

{-|
-}
unwords : [String] -> String
unwords = Native.unwords

{-|
-}
lines : String -> [String]
lines = Native.lines

{-|
-}
unlines : [String] -> String
unlines = Native.unlines

{-|
-}
toUpper : String -> String
toUpper = Native.toUpper

{-|
-}
toLower : String -> String
toLower = Native.toLower

{-|
-}
any : (Char -> Bool) -> String -> Bool
any = Native.any

{-|
-}
all : (Char -> Bool) -> String -> Bool
all = Native.all
