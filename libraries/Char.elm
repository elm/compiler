
module Char where

import Native.Char as N

-- Selects upper case letters.
isUpper : Char -> Bool

-- Selects lower case letters.
isLower : Char -> Bool

-- Selects ASCII digits (0..9).
isDigit : Char -> Bool

-- Selects ASCII octal digits (0..7).
isOctDigit : Char -> Bool

-- Selects ASCII hexadecimal digits (0..9a..fA..F).
isHexDigit : Char -> Bool

-- Convert to upper case.
toUpper : Char -> Char

-- Convert to lower case.
toLower : Char -> Char

-- Convert to upper case, according to any locale-specific case mappings.
toLocaleUpper : Char -> Char

-- Convert to lower case, according to any locale-specific case mappings.
toLocaleLower : Char -> Char

-- Convert to unicode.
toCode : Char -> Int

-- Convert from unicode.
fromCode : Int -> Char

