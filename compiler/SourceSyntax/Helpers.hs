
module SourceSyntax.Helpers where

import Data.Char (isSymbol,isDigit)

brkt s = "{ " ++ s ++ " }"

isTuple name =
    take 6 name == "_Tuple" && all isDigit (drop 6 name)

isOp c =
    isSymbol c || elem c "+-/*=.$<>:&|^?%#@~!"