
module SourceSyntax.Helpers where

import Data.Char (isSymbol)

brkt s = "{ " ++ s ++ " }"

parens s = "(" ++ s ++ ")"
parensIf b s = if b then parens s else s

isOp c = isSymbol c || elem c "+-/*=.$<>:&|^?%#@~!"