
module Char where

import Native.Char as Native

-- True for upper case letters.
isUpper : Char -> Bool
isUpper = Native.isUpper

-- True for lower case letters.
isLower : Char -> Bool
isLower = Native.isLower

-- True for ASCII digits (`0..9`).
isDigit : Char -> Bool
isDigit = Native.isDigit

-- True for ASCII octal digits (`0..7`).
isOctDigit : Char -> Bool
isOctDigit = Native.isOctDigit

-- True for ASCII hexadecimal digits (`0..9a..fA..F`).
isHexDigit : Char -> Bool
isHexDigit = Native.isHexDigit

-- Convert to upper case.
toUpper : Char -> Char
toUpper = Native.toUpper

-- Convert to lower case.
toLower : Char -> Char
toLower = Native.toLower

-- Convert to upper case, according to any locale-specific case mappings.
toLocaleUpper : Char -> Char
toLocaleUpper = Native.toLocaleUpper

-- Convert to lower case, according to any locale-specific case mappings.
toLocaleLower : Char -> Char
toLocaleLower = Native.toLocaleLower

type KeyCode = Int

-- Convert to unicode.
toCode : Char -> KeyCode
toCode = Native.toCode

-- Convert from unicode.
fromCode : KeyCode -> Char
fromCode = Native.fromCode
