{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Helpers
  ( zeroTuple
  , makeTuple
  , isTuple
  , isOp
  , isSymbol
  )
  where

import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)



-- TUPLES


zeroTuple :: Text
zeroTuple =
  "_Tuple0"


makeTuple :: Int -> Text
makeTuple size =
  Text.pack ("_Tuple" ++ show size)


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
  Set.fromList "+-/*=.<>:&|^?%~!"
