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
        | 'x' `elem` n         = L.IntNum (read n)
        | any (`elem` ".eE") n = L.FloatNum (read n)
        | otherwise            = L.IntNum (read n)    

    number = concat <$> sequence
             [ option "" minus
             , hexNum <|> decNum ]

    decNum = concat <$> sequence
             [ many1 digit
             , option "" decimals
             , option "" exponent ]

    hexNum = do
      try $ lookAhead (string "0x" >> hexDigit)
      string "0x"
      n <- many1 hexDigit
      return $ "0x" ++ n

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
