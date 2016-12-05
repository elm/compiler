{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Helpers where

import qualified Data.Char as Char
import qualified Data.Text as Text
import Data.Text (Text)



-- TUPLES


isTuple :: Text -> Bool
isTuple name =
  Text.isPrefixOf "_Tuple" name
  &&
  Text.all Char.isDigit (Text.drop 6 name)



-- INFIX OPERATORS


isOp :: String -> Bool
isOp name =
  all isSymbol name


isSymbol :: Char -> Bool
isSymbol c =
  elem c ("+-/*=.<>:&|^?%#@~!" :: String)
