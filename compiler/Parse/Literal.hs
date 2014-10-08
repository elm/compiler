{-# OPTIONS_GHC -W #-}
module Parse.Literal (literal) where

import Control.Applicative ((<$>))
import Text.Parsec hiding (newline,spaces)
import Parse.Helpers (IParser, chr, str)
import qualified AST.Literal as L

literal :: IParser L.Literal
literal = num <|> (L.Str <$> str) <|> (L.Chr <$> chr) <?> "literal"

num :: IParser L.Literal
num = toLit <$> (number <?> "number")
  where
    toLit n
        | any (`elem` ".eE") n = L.FloatNum (read n)
        | otherwise            = L.IntNum (read n)    

    number = concat <$> sequence
             [ option "" minus
             , many1 digit
             , option "" decimals
             , option "" exponent ]

    minus = try $ do
              string "-"
              lookAhead digit
              return "-"

    decimals = do
      try $ lookAhead (string "." >> digit)
      string "."
      n <- many1 digit
      return ('.' : n)

    exponent = do
      string "e" <|> string "E"
      op <- option "" (string "+" <|> string "-")
      n <- many1 digit
      return ('e' : op ++ n)
