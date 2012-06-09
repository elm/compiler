
module ParseLib where

import Ast
import Control.Monad
import Data.Char (isSymbol)
import Text.Parsec

reserved = [ "if", "then", "else"
           , "case", "of", "data"
           , "let", "in" ]

expecting = flip (<?>)

var :: (Monad m) => ParsecT [Char] u m String
var = do
  c <- letter {-lower-} <|> char '_'
  cs <- many $ alphaNum <|> char '_' <|> char '\''
  guard $ c:cs `notElem` reserved
  return $ c:cs

anyOp :: (Monad m) => ParsecT [Char] u m String
anyOp = many1 (satisfy isSymbol <|> oneOf "+-/*=.$<>:&|^?%#@~!")

lexeme p = do e <- p ; whitespace ; return e

whitespace :: (Monad m) => ParsecT [Char] u m String
whitespace = try $ many (char ' ')

betwixt a b = between (char a) (char b <?> "closing '" ++ [b] ++ "'")
betwixtSpcs a b = between (lexeme $ char a) (char b <?> "closing '" ++ [b] ++ "'")

listOf p = betwixtSpcs '[' ']' $ p `sepBy` lexeme (char ',')

accessible expr = do
  e <- expr
  access <- optionMaybe (char '.')
  case access of
    Just '.' -> accessible (Access e `liftM` var)
    Nothing -> return e
