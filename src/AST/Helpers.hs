{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Helpers
  ( zeroTuple
  , makeTuple
  , isTuple
  , isOp
  , isSymbol
  , desymbol
  )
  where

import qualified Data.Char as Char
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Text as Text
import Data.Text (Text)



-- TUPLES


zeroTuple :: Text
zeroTuple =
  "#0"


makeTuple :: Int -> Text
makeTuple size =
  Text.pack ('#' : show size)


isTuple :: Text -> Bool
isTuple name =
  Text.isPrefixOf "#" name
  &&
  Text.all Char.isDigit (Text.drop 1 name)



-- INFIX OPERATORS


isOp :: Text -> Bool
isOp name =
  Text.all isSymbol name


isSymbol :: Char -> Bool
isSymbol c =
  Map.member c validSymbols


validSymbols :: Map.Map Char Char
validSymbols =
  Map.fromList $
    zip
      "+-/*=.<>:&|^?%~!"
      "abcdefghijklmnop"


desymbol :: Text -> Text
desymbol name =
  Text.map (\c -> validSymbols ! c) name
