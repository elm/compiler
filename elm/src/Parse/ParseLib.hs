
module ParseLib where

import Ast
import Control.Applicative ((<$>))
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
var = makeVar (letter <|> char '_' <?> "variable")

lowVar :: (Monad m) => ParsecT [Char] u m String
lowVar = makeVar (lower <?> "lower case variable")
capVar :: (Monad m) => ParsecT [Char] u m String
capVar = makeVar (upper <?> "upper case variable")

makeVar p = do
  c <- p
  cs <- many (alphaNum <|> char '_' <|> char '\'' <?> "")
  guard $ c:cs `notElem` reserved
  return $ c:cs


anyOp :: (Monad m) => ParsecT [Char] u m String
anyOp = betwixt '`' '`' var <|>
        (do op <- many1 (satisfy isSymbol <|> oneOf "+-/*=.$<>:&|^?%#@~!")
            guard (op `notElem` [ "=", "..", "->", "--" ])
            return op) <?> "infix operator"

arrow :: (Monad m) => ParsecT [Char] u m String
arrow = string "->" <|> string "\8594" <?> "arrow (->)"

commaSep :: (Monad m) => ParsecT [Char] u m a -> ParsecT [Char] u m [a]
commaSep p = do
  x <- optionMaybe p
  case x of
    Just a  -> (a:) <$> many (try (whitespace >> char ',' >> whitespace >> p))
    Nothing -> return []

semiSep1 :: (Monad m) => ParsecT [Char] u m a -> ParsecT [Char] u m [a]
semiSep1 p = do a <- p
                (a:) <$> many (try (whitespace >> char ';' >> whitespace >> p))

pipeSep1 :: (Monad m) => ParsecT [Char] u m a -> ParsecT [Char] u m [a]
pipeSep1 p = do a <- p
                (a:) <$> many (try (whitespace >> char '|' >> whitespace >> p))

spaceSep1 :: (Monad m) => ParsecT [Char] u m a -> ParsecT [Char] u m [a]
spaceSep1 p = do a <- p
                 (a:) <$> many (try (forcedWS >> p))

betwixt a b c = do char a ; out <- c
                   char b <?> "closing '" ++ [b] ++ "'" ; return out

surround a z name p = do
  char a ; whitespace ; a <- p ; whitespace
  char z <?> unwords ["closing", name, show z]
  return a

braces   :: (Monad m) => ParsecT [Char] u m a -> ParsecT [Char] u m a
braces   = surround '[' ']' "brace"

parens   :: (Monad m) => ParsecT [Char] u m a -> ParsecT [Char] u m a
parens   = surround '(' ')' "paren"

brackets :: (Monad m) => ParsecT [Char] u m a -> ParsecT [Char] u m a
brackets = surround '{' '}' "bracket"

accessible expr = do
  e <- expr
  access <- optionMaybe . try $
                  (do { char '.' ; notFollowedBy (char '.') } <?> "field access (e.g. List.map)")
  case access of
    Just _  -> accessible (Access e `liftM` var <?> "field access (e.g. List.map)")
    Nothing -> return e


spaces :: (Monad m) => ParsecT [Char] u m String
spaces = many1 ((multiComment <|> string " " <?> "") >> return ' ') <?> "spaces"

forcedWS :: (Monad m) => ParsecT [Char] u m [String]
forcedWS = try (do { spaces; many nl_space }) <|> try (many1 nl_space)
    where nl_space = try $ many1 newline >> spaces

whitespace :: (Monad m) => ParsecT [Char] u m ()
whitespace = optional forcedWS <?> ""

freshLine :: (Monad m) => ParsecT [Char] u m [[String]]
freshLine = try (do { many1 newline; many space_nl }) <|> try (many1 space_nl) <?> ""
    where space_nl = try $ spaces >> many1 newline

newline :: (Monad m) => ParsecT [Char] u m String
newline = simpleNewline <|> lineComment <?> ""

simpleNewline :: (Monad m) => ParsecT [Char] u m String
simpleNewline = try (string "\r\n") <|> string "\n"

lineComment :: (Monad m) => ParsecT [Char] u m String
lineComment = do try (string "--")
                 manyTill anyChar $ simpleNewline <|> (eof >> return "\n")

multiComment :: (Monad m) => ParsecT [Char] u m String
multiComment = do { try (string "{-"); closeComment }

closeComment :: (Monad m) => ParsecT [Char] u m String
closeComment = manyTill anyChar . choice $
               [ try (string "-}") <?> "close comment"
               , do { try $ string "{-"; closeComment; closeComment }
               ]