{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Helpers (isTuple, isOp, isSymbol) where

import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)



-- TUPLES


isTuple :: Text -> Bool
isTuple name =
  Text.isPrefixOf "_Tuple" name
  &&
  Text.all Char.isDigit (Text.drop 6 name)



-- INFIX OPERATORS


isOp :: Text -> Bool
isOp name =
  Text.all isSymbol name


isSymbol :: Char -> Bool
isSymbol c =
  Set.member c validSymbols


validSymbols :: Set.Set Char
validSymbols =
  Set.fromList "+-/*=.<>:&|^?%#@~!"
