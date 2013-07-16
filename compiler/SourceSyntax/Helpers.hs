
module SourceSyntax.Helpers where

import qualified Data.Char as Char

brkt s = "{ " ++ s ++ " }"

isTuple name =
    take 6 name == "_Tuple" && all Char.isDigit (drop 6 name)

isOp = all isSymbol

isSymbol c =
    Char.isSymbol c || elem c "+-/*=.$<>:&|^?%#@~!"