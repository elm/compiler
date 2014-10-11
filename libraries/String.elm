module String where
{-| A built-in representation for efficient string manipulation. String literals
are enclosed in `"double quotes"`. Strings are *not* lists of characters.

# Basics
@docs isEmpty, length, reverse, repeat

# Building and Splitting
@docs cons, uncons, append, concat, split, join, words, lines

# Get Substrings
@docs slice, left, right, dropLeft, dropRight

# Check for Substrings
@docs contains, startsWith, endsWith, indexes, indices

# Conversions
@docs show, toInt, toFloat, toList, fromList

# Formatting
Cosmetic operations such as padding with extra characters or trimming whitespace.

@docs toUpper, toLower,
      pad, padLeft, padRight,
      trim, trimLeft, trimRight

# Higher-Order Functions
@docs map, filter, foldl, foldr, any, all
-}

import Native.Show
import Native.String
import Maybe (Maybe)

{-| Determine if a string is empty.

      isEmpty "" == True
      isEmpty "the world" == False
-}
isEmpty : String -> Bool
isEmpty = Native.String.isEmpty

{-| Add a character to the beginning of a string. -}
cons : Char -> String -> String
cons = Native.String.cons

{-| Split a non-empty string into its head and tail. This lets you
pattern match on strings exactly as you would with lists.

      uncons "abc" == Just ('a',"bc")
      uncons ""    == Nothing
-}
uncons : String -> Maybe (Char, String)
uncons = Native.String.uncons

{-| Append two strings. You can also use [the `(++)` operator](/library/List.elm#++)
to do this.

      append "butter" "fly" == "butterfly"
-}
append : String -> String -> String
append = Native.String.append

{-| Concatenate many strings into one.

      concat ["never","the","less"] == "nevertheless"
-}
concat : [String] -> String
concat = Native.String.concat

{-| Get the length of a string.

      length "innumerable" == 11
      length "" == 0

-}
length : String -> Int
length = Native.String.length

{-| Transform every character in a string

      map (\c -> if c == '/' then '.' else c) "a/b/c" == "a.b.c"
-}
map : (Char -> Char) -> String -> String
map = Native.String.map

{-| Keep only the characters that satisfy the predicate.

      filter isDigit "R2-D2" == "22"
-}
filter : (Char -> Bool) -> String -> String
filter = Native.String.filter

{-| Reverse a string.

      reverse "stressed" == "desserts"
-}
reverse : String -> String
reverse = Native.String.reverse

{-| Reduce a string from the left.

      foldl cons "" "time" == "emit"
-}
foldl : (Char -> b -> b) -> b -> String -> b
foldl = Native.String.foldl

{-| Reduce a string from the right.

      foldr cons "" "time" == "time"
-}
foldr : (Char -> b -> b) -> b -> String -> b
foldr = Native.String.foldr

{-| Split a string using a given separator.

      split "," "cat,dog,cow"        == ["cat","dog","cow"]
      split "/" "home/evan/Desktop/" == ["home","evan","Desktop", ""]

Use `Regex.split` if you need something more flexible.
-}
split : String -> String -> [String]
split = Native.String.split

{-| Put many strings together with a given separator.

      join "a" ["H","w","ii","n"]        == "Hawaiian"
      join " " ["cat","dog","cow"]       == "cat dog cow"
      join "/" ["home","evan","Desktop"] == "home/evan/Desktop"
-}
join : String -> [String] -> String
join = Native.String.join

{-| Repeat a string *n* times.

      repeat 3 "ha" == "hahaha"
-}
repeat : Int -> String -> String
repeat = Native.String.repeat

{-| Take a substring given a start and end index. Negative indexes
are taken starting from the *end* of the list.

      slice  7  9 "snakes on a plane!" == "on"
      slice  0  6 "snakes on a plane!" == "snakes"
      slice  0 -7 "snakes on a plane!" == "snakes on a"
      slice -6 -1 "snakes on a plane!" == "plane"
-}
slice : Int -> Int -> String -> String
slice = Native.String.slice

{-| Take *n* characters from the left side of a string. -}
left : Int -> String -> String
left = Native.String.left

{-| Take *n* characters from the right side of a string. -}
right : Int -> String -> String
right = Native.String.right

{-| Drop *n* characters from the left side of a string. -}
dropLeft : Int -> String -> String
dropLeft = Native.String.dropLeft

{-| Drop *n* characters from the right side of a string. -}
dropRight : Int -> String -> String
dropRight = Native.String.dropRight

{-| Pad a string on both sides until it has a given length.

      pad 5 ' ' "1"   == "  1  "
      pad 5 ' ' "11"  == "  11 "
      pad 5 ' ' "121" == " 121 "
-}
pad : Int -> Char -> String -> String
pad = Native.String.pad

{-| Pad a string on the left until it has a given length.

      padLeft 5 '.' "1"   == "....1"
      padLeft 5 '.' "11"  == "...11"
      padLeft 5 '.' "121" == "..121"
-}
padLeft : Int -> Char -> String -> String
padLeft = Native.String.padLeft

{-| Pad a string on the right until it has a given length.

      padRight 5 '.' "1"   == "1...."
      padRight 5 '.' "11"  == "11..."
      padRight 5 '.' "121" == "121.."
-}
padRight : Int -> Char -> String -> String
padRight = Native.String.padRight

{-| Get rid of whitespace on both sides of a string.

      trim "  hats  \n" == "hats"
-}
trim : String -> String
trim = Native.String.trim

{-| Get rid of whitespace on the left of a string.

      trimLeft "  hats  \n" == "hats  \n"
-}
trimLeft : String -> String
trimLeft = Native.String.trimLeft

{-| Get rid of whitespace on the right of a string.

      trimRight "  hats  \n" == "  hats"
-}
trimRight : String -> String
trimRight = Native.String.trimRight

{-| Break a string into words, splitting on chunks of whitespace.

      words "How are \t you? \n Good?" == ["How","are","you?","Good?"]
-}
words : String -> [String]
words = Native.String.words

{-| Break a string into lines, splitting on newlines.

      lines "How are you?\nGood?" == ["How are you?", "Good?"]
-}
lines : String -> [String]
lines = Native.String.lines

{-| Convert a string to all upper case. Useful for case insensitive comparisons
and VIRTUAL YELLING.
-}
toUpper : String -> String
toUpper = Native.String.toUpper

{-| Convert a string to all lower case. Useful for case insensitive comparisons. -}
toLower : String -> String
toLower = Native.String.toLower

{-| Determine whether *any* characters satisfy a predicate.

      any isDigit "90210" == True
      any isDigit "R2-D2" == True
      any isDigit "heart" == False
-}
any : (Char -> Bool) -> String -> Bool
any = Native.String.any

{-| Determine whether *all* characters satisfy a predicate.

      all isDigit "90210" == True
      all isDigit "R2-D2" == False
      all isDigit "heart" == False
-}
all : (Char -> Bool) -> String -> Bool
all = Native.String.all

{-| See if the second string contains the first one.

      contains "the" "theory" == True
      contains "hat" "theory" == False
      contains "THE" "theory" == False

Use `Regex.contains` if you need something more flexible.
-}
contains : String -> String -> Bool
contains = Native.String.contains

{-| See if the second string starts with the first one.

      startsWith "the" "theory" == True
      startsWith "ory" "theory" == False
-}
startsWith : String -> String -> Bool
startsWith = Native.String.startsWith

{-| See if the second string ends with the first one.

      endsWith "the" "theory" == False
      endsWith "ory" "theory" == True
-}
endsWith : String -> String -> Bool
endsWith = Native.String.endsWith

{-| Get all of the indexes for a substring in another string.

      indexes "i" "Mississippi"   == [1,4,7,10]
      indexes "ss" "Mississippi"  == [2,5]
      indexes "needle" "haystack" == []
-}
indexes : String -> String -> [Int]
indexes = Native.String.indexes

{-| Alias for `indexes` -}
indices : String -> String -> [Int]
indices = Native.String.indexes

{-| Turn any kind of value into a string.

      show 42    == "42"
      show [1,2] == "[1,2]"
-}
show : a -> String
show = Native.Show.show

{-| Try to convert a string into an int, failing on improperly formatted strings.

      toInt "123" == Just 123
      toInt "-42" == Just -42
      toInt "3.1" == Nothing
      toInt "31a" == Nothing
-}
toInt : String -> Maybe Int
toInt = Native.String.toInt

{-| Try to convert a string into a float, failing on improperly formatted strings.

      toFloat "123" == Just 123.0
      toFloat "-42" == Just -42.0
      toFloat "3.1" == Just 3.1
      toFloat "31a" == Nothing
-}
toFloat : String -> Maybe Float
toFloat = Native.String.toFloat

{-| Convert a string to a list of characters.

      toList "abc" == ['a','b','c']
-}
toList : String -> [Char]
toList = Native.String.toList

{-| Convert a list of characters into a String. Can be useful if you
want to create a string primarly by consing, perhaps for decoding
something.

      fromList ['a','b','c'] == "abc"
-}
fromList : [Char] -> String
fromList = Native.String.fromList
