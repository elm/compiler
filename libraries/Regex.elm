module Regex where
{-| A library for working with regular expressions. It uses [the
same kind of regular expressions accepted by JavaScript](https://developer.mozilla.org/en/docs/Web/JavaScript/Guide/Regular_Expressions).

# Create
@docs regex, escape, caseInsensitive

# Helpful Data Structures

These data structures are needed to help define functions like [`find`](#find)
and [`replace`](#replace).

@docs HowMany, Match

# Use
@docs contains, find, replace, split

-}

import Maybe (Maybe)
import Native.Regex

data Regex = Regex

{-| Escape strings to be regular expressions, making all special characters
safe. So `regex (escape "^a+")` will match exactly `"^a+"` instead of a series
of `a`&rsquo;s that start at the beginning of the line.
-}
escape : String -> String
escape = Native.Regex.escape

{-| Create a Regex that matches patterns [as specified in JavaScript](https://developer.mozilla.org/en/docs/Web/JavaScript/Guide/Regular_Expressions#Writing_a_Regular_Expression_Pattern).

Be careful to escape backslashes properly! For example, `"\w"` is escaping the
letter `w` which is probably not what you want. You probably want `"\\w"`
instead, which escapes the backslash.
-}
regex : String -> Regex
regex = Native.Regex.regex


{-| Make a regex case insensitive -}
caseInsensitive : Regex -> Regex
caseInsensitive = Native.Regex.caseInsensitive

{-| Check to see if a Regex is contained in a string.

```haskell
  contains (regex "123") "12345" == True
  contains (regex "b+") "aabbcc" == True

  contains (regex "789") "12345" == False
  contains (regex "z+") "aabbcc" == False
```
-}
contains : Regex -> String -> Bool
contains = Native.Regex.contains

{-| A `Match` represents all of the details about a particular match in a string.
Here are details on each field:

  * `match` &mdash; the full string of the match.
  * `submatches` &mdash; a regex might have [subpatterns, surrounded by
    parentheses](https://developer.mozilla.org/en/docs/Web/JavaScript/Guide/Regular_Expressions#Using_Parenthesized_Substring_Matches).
    If there are N subpatterns, there will be N elements in the `submatches` list.
    Each submatch in this list is a `Maybe` because not all subpatterns may trigger.
    For example, `(regex "(a+)|(b+)")` will either match many `a`&rsquo;s or
    many `b`&rsquo;s, but never both.
  * `index` &mdash; the index of the match in the original string.
  * `number` &mdash; if you find many matches, you can think of each one
    as being labeled with a `number` starting at one. So the first time you
    find a match, that is match `number` one. Second time is match `number` two.
    This is useful when paired with `replace All` if replacement is dependent on how
    many times a pattern has appeared before.
-}
type Match = { match : String, submatches : [Maybe String], index : Int, number : Int }

{-| `HowMany` is used to specify how many matches you want to make. So
`replace All` would replace every match, but `replace (AtMost 2)` would
replace at most two matches (i.e. zero, one, two, but never three or more).
-}
data HowMany = All | AtMost Int

{-| Find matches in a string:

```haskell
  findTwoCommas = find (AtMost 2) (regex ",")

    -- map .index (findTwoCommas "a,b,c,d,e") == [1,3]
    -- map .index (findTwoCommas "a b c d e") == []

  places = find All (regex "[oi]n a (\\w+)") "I am on a boat in a lake."

    -- map .match places == ["on a boat", "in a lake"]
    -- map .submatches places == [ [Just "boat"], [Just "lake"] ]
```
-}
find : HowMany -> Regex -> String -> [Match]
find = Native.Regex.find

{-| Replace matches. The function from `Match` to `String` lets
you use the details of a specific match when making replacements.

```haskell
  devowel = replace All (regex "[aeiou]") (\_ -> "")

    -- devowel "The quick brown fox" == "Th qck brwn fx"

  reverseWords = replace All (regex "\\w+") (\{match} -> String.reverse match)

    -- reverseWords "deliver mined parts" == "reviled denim strap"
```
-}
replace : HowMany -> Regex -> (Match -> String) -> String -> String
replace = Native.Regex.replace

{-| Split a string, using the regex as the separator.

```haskell
  split (AtMost 1) (regex ",") "tom,99,90,85" == ["tom","99,90,85"]

  split All (regex ",") "a,b,c,d" == ["a","b","c","d"]
```
-}
split : HowMany -> Regex -> String -> [String]
split = Native.Regex.split
