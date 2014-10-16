
module Char where

{-| Functions for working with characters. Character literals are enclosed in
`'a'` pair of single quotes.

# Classification
@docs isUpper, isLower, isDigit, isOctDigit, isHexDigit

# Conversion
@docs toUpper, toLower, toLocaleUpper, toLocaleLower, toCode, fromCode

-}

import Native.Char

{-| True for upper case letters. -}
isUpper : Char -> Bool
isUpper = Native.Char.isUpper

{-| True for lower case letters. -}
isLower : Char -> Bool
isLower = Native.Char.isLower

{-| True for ASCII digits `[0-9]`. -}
isDigit : Char -> Bool
isDigit = Native.Char.isDigit

{-| True for ASCII octal digits `[0-7]`. -}
isOctDigit : Char -> Bool
isOctDigit = Native.Char.isOctDigit

{-| True for ASCII hexadecimal digits `[0-9a-fA-F]`. -}
isHexDigit : Char -> Bool
isHexDigit = Native.Char.isHexDigit

{-| Convert to upper case. -}
toUpper : Char -> Char
toUpper = Native.Char.toUpper

{-| Convert to lower case. -}
toLower : Char -> Char
toLower = Native.Char.toLower

{-| Convert to upper case, according to any locale-specific case mappings. -}
toLocaleUpper : Char -> Char
toLocaleUpper = Native.Char.toLocaleUpper

{-| Convert to lower case, according to any locale-specific case mappings. -}
toLocaleLower : Char -> Char
toLocaleLower = Native.Char.toLocaleLower

type alias KeyCode = Int

{-| Convert to unicode. Used with the `Keyboard` library, which expects the
input to be uppercase. -}
toCode : Char -> KeyCode
toCode = Native.Char.toCode

{-| Convert from unicode. -}
fromCode : KeyCode -> Char
fromCode = Native.Char.fromCode
