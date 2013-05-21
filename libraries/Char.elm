
module Char where

import Native.Char as N

-- True for upper case letters.
isUpper : Char -> Bool

-- True for lower case letters.
isLower : Char -> Bool

-- True for ASCII digits (`0..9`).
isDigit : Char -> Bool

-- True for ASCII octal digits (`0..7`).
isOctDigit : Char -> Bool

-- True for ASCII hexadecimal digits (`0..9a..fA..F`).
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
toCode : Char -> KeyCode

-- Convert from unicode.
fromCode : KeyCode -> Char

