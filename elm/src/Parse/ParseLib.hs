
module ParseLib where

import Ast
import Control.Monad
import Data.Char (isSymbol)
import Text.Parsec hiding (newline,spaces)

reserved = [ "if", "then", "else"
           , "case", "of", "data"
           , "let", "in" ]

expecting = flip (<?>)

backslashed :: (Monad m) => ParsecT [Char] u m Char
backslashed = do { char '\\'; c <- anyChar
                 ; return . read $ ['\'','\\',c,'\''] }

var :: (Monad m) => ParsecT [Char] u m String
var = makeVar $ letter <|> char '_'

lowVar :: (Monad m) => ParsecT [Char] u m String
lowVar = makeVar (lower <?> "lower case variable")
capVar :: (Monad m) => ParsecT [Char] u m String
capVar = makeVar (upper <?> "upper case variable")

makeVar p = do
  c <- p
  cs <- many $ alphaNum <|> char '_' <|> char '\''
  guard $ c:cs `notElem` reserved
  return $ c:cs


anyOp :: (Monad m) => ParsecT [Char] u m String
anyOp = betwixt '`' '`' var <|>
        (do op <- many1 (satisfy isSymbol <|> oneOf "+-/*=.$<>:&|^?%#@~!")
            guard (op `notElem` [ "=", "..", "->" ])
            return op) <?> "infix operator"

lexeme p = do whitespace ; p

symbol :: (Monad m) => String -> ParsecT [Char] u m String
symbol s = whitespace >> string s

arrow :: (Monad m) => ParsecT [Char] u m String
arrow = symbol "->" <|> symbol "\8594"

commaSep p = p `sepBy` lexeme (char ',')

             
betwixt a b c = do char a ; out <- c
                   char b <?> "closing '" ++ [b] ++ "'" ; return out
betwixtSpcs a b c = do char a ; whitespace ; out <- c ; whitespace
                       char b <?> "closing '" ++ [b] ++ "'" ; return out

listOf p = betwixtSpcs '[' ']' $ p `sepBy` lexeme (char ',')

accessible expr = do
  e <- expr
  access <- optionMaybe (char '.')
  case access of
    Just '.' -> accessible (Access e `liftM` var)
    Nothing -> return e


spaces :: (Monad m) => ParsecT [Char] u m String
spaces = many1 ((multiComment <|> string " ") >> return ' ') <?> "spaces"

forcedWS :: (Monad m) => ParsecT [Char] u m [String]
forcedWS = try (do { spaces; many nl_space }) <|> try (many1 nl_space)
    where nl_space = try $ many1 newline >> spaces

whitespace :: (Monad m) => ParsecT [Char] u m ()
whitespace = optional forcedWS <?> "whitespace"

freshLine :: (Monad m) => ParsecT [Char] u m [[String]]
freshLine = try (do { many1 newline; many space_nl }) <|> try (many1 space_nl) <?> "fresh line"
    where space_nl = try $ spaces >> many1 newline

newline :: (Monad m) => ParsecT [Char] u m String
newline = simpleNewline <|> lineComment <?> "newline"

simpleNewline :: (Monad m) => ParsecT [Char] u m String
simpleNewline = try (string "\r\n") <|> string "\n"

lineComment :: (Monad m) => ParsecT [Char] u m String
lineComment = do try $ string "--"
                 manyTill anyChar $ simpleNewline <|> (eof >> return "\n")

multiComment :: (Monad m) => ParsecT [Char] u m String
multiComment = do { try $ string "{-"; closeComment }

closeComment :: (Monad m) => ParsecT [Char] u m String
closeComment = manyTill anyChar . choice $
               [ try $ string "-}"
               , do { try $ string "{-"; closeComment; closeComment }
               ]