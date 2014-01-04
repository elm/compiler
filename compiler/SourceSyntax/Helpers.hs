{-# OPTIONS_GHC -Wall #-}
module SourceSyntax.Helpers where

import qualified Data.Char as Char

brkt :: String -> String
brkt s = "{ " ++ s ++ " }"

isTuple :: String -> Bool
isTuple name =
    take 6 name == "_Tuple" && all Char.isDigit (drop 6 name)

isOp :: String -> Bool
isOp = all isSymbol

isSymbol :: Char -> Bool
isSymbol c =
    Char.isSymbol c || elem c "+-/*=.$<>:&|^?%#@~!"
