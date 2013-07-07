
module SourceSyntax.Helpers where

import Data.Char (isSymbol,isDigit)

brkt s = "{ " ++ s ++ " }"

isTuple name =
    take 5 name == "Tuple" && all isDigit (drop 5 name)

isOp c =
    isSymbol c || elem c "+-/*=.$<>:&|^?%#@~!"