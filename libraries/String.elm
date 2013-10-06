module String where
{-| A built-in representation of String for effecient string manipulation.

@docs isEmpty, length, cons, uncons

@docs map, filter, reverse, foldl, foldr

@docs any, all, toUpper, toLower

@docs split, join, repeat

Should the next 4 exist?
@docs left, right, dropLeft, dropRight, sub

@docs pad, padLeft, padRight

@docs trim, trimLeft, trimRight

@docs words, unwords, lines, unlines

@docs contains, startsWith, endsWith, indices, indexes

@docs toInt, toFloat

-}

import Native.String
import Maybe (Maybe)

{-|
-}
isEmpty : String
isEmpty = Native.String.isEmpty

{-|
-}
cons : Char -> String -> String
cons = Native.String.cons

{-|
-}
uncons : String -> Maybe (Char, String)
uncons = Native.String.uncons

{-|
-}
length : String -> Int
length = Native.String.length

{-|
-}
map : (Char -> Char) -> String -> String
map = Native.String.map

{-|
-}
filter : (Char -> Bool) -> String -> String
filter = Native.String.filter

{-|
-}
reverse : String -> String
reverse = Native.String.reverse

{-|
-}
foldl : (Char -> b -> b) -> b -> String -> b
foldl = Native.String.foldl

{-|
-}
foldr : (Char -> b -> b) -> b -> String -> b
foldr = Native.String.foldr

{-|
-}
split : String -> String -> [String]
split = Native.String.split

{-|
-}
join : String -> [String] -> String
join = Native.String.join

{-|
-}
repeat : Int -> String -> String
repeat = Native.String.repeat

sub : Int -> Int -> String -> String
sub = Native.String.sub

{-|
-}
left : Int -> String -> String
left = Native.String.left

{-|
-}
right : Int -> String -> String
right = Native.String.right

{-|
-}
dropLeft : Int -> String -> String
dropLeft = Native.String.dropLeft

{-|
-}
dropRight : Int -> String -> String
dropRight = Native.String.dropRight

{-|
-}
pad : Int -> Char -> String -> String
pad = Native.String.pad

{-|
-}
padLeft : Int -> Char -> String -> String
padLeft = Native.String.padLeft

{-|
-}
padRight : Int -> Char -> String -> String
padRight = Native.String.padRight

{-|
-}
trim : String -> String
trim = Native.String.trim

{-|
-}
trimLeft : String -> String
trimLeft = Native.String.trimLeft

{-|
-}
trimRight : String -> String
trimRight = Native.String.trimRight

{-|
-}
words : String -> [String]
words = Native.String.words

{-|
-}
unwords : [String] -> String
unwords = Native.String.unwords

{-|
-}
lines : String -> [String]
lines = Native.String.lines

{-|
-}
unlines : [String] -> String
unlines = Native.String.unlines

{-|
-}
toUpper : String -> String
toUpper = Native.String.toUpper

{-|
-}
toLower : String -> String
toLower = Native.String.toLower

{-|
-}
any : (Char -> Bool) -> String -> Bool
any = Native.String.any

{-|
-}
all : (Char -> Bool) -> String -> Bool
all = Native.String.all

{-|
-}
contains : String -> String -> Bool
contains = Native.String.contains

{-|
-}
startsWith : String -> String -> Bool
startsWith = Native.String.startsWith

{-|
-}
endsWith : String -> String -> Bool
endsWith = Native.String.endsWith

{-|
-}
indices : String -> String -> [Int]
indices = Native.String.indexes

{-|
-}
indexes : String -> String -> [Int]
indexes = Native.String.indexes

{-|
-}
toInt : String -> Maybe Int
toInt = Native.String.toInt

{-|
-}
toFloat : String -> Maybe Float
toFloat = Native.String.toFloat
