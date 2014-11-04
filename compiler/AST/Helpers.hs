{-# OPTIONS_GHC -Wall #-}
module AST.Helpers where

import qualified Data.Char as Char

splitDots :: String -> [String]
splitDots = go []
  where
    go vars str =
        case break (=='.') str of
          (x,_:rest) | isOp x -> vars ++ [x ++ '.' : rest]
                     | otherwise -> go (vars ++ [x]) rest
          (x,[]) -> vars ++ [x]

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
