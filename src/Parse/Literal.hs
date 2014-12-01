module Parse.Literal (literal) where

import Prelude hiding (exponent)
import Control.Applicative ((<$>))
import Text.Parsec ((<|>), (<?>), digit, hexDigit, lookAhead, many1, option, string, try)
import Parse.Helpers (IParser, chr, str)
import qualified AST.Literal as L


literal :: IParser L.Literal
literal =
  num <|> (L.Str <$> str) <|> (L.Chr <$> chr) <?> "literal"


num :: IParser L.Literal
num =
  toLiteral <$> (rawNumber <?> "number")


toLiteral :: String -> L.Literal
toLiteral n
  | 'x' `elem` n         = L.IntNum (read n)    
  | any (`elem` ".eE") n = L.FloatNum (read n)
  | otherwise            = L.IntNum (read n)    


rawNumber :: IParser String
rawNumber =
  concat <$> sequence
    [ option "" minus
    , base16 <|> base10
    ]


base16 :: IParser String
base16 =
  do  try (string "0x")
      digits <- many1 hexDigit
      return ("0x" ++ digits)


base10 :: IParser String
base10 =
  concat <$> sequence
    [ many1 digit
    , option "" decimals
    , option "" exponent
    ]


minus :: IParser String
minus =
  try $ do
    string "-"
    lookAhead digit
    return "-"


decimals :: IParser String
decimals =
  do  try $ lookAhead (string "." >> digit)
      string "."
      n <- many1 digit
      return ('.' : n)


exponent :: IParser String
exponent =
  do  string "e" <|> string "E"
      op <- option "" (string "+" <|> string "-")
      n <- many1 digit
      return ('e' : op ++ n)
