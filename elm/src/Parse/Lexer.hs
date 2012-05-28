
module Lexer (tokenize) where

import Control.Applicative ((<|>), (<$>), (<*>))
import Data.Char (isSymbol)
import Text.Parsec (Parsec, alphaNum, anyChar, char, choice, digit, eof, letter,
                    many, many1, manyTill, newline, notFollowedBy, oneOf, parse,
                    satisfy, string, try, upper)
import Tokens
import Control.Monad (liftM)

token :: Parsec String u Token
token = NUMBER <$> integer
    <|> whitespace
    <|> chrs [ ('(',LPAREN)  , (')',RPAREN)
             , ('{',LBRACE)  , ('}',RBRACE)
             , ('[',LBRACKET), (']',RBRACKET)
             , (',',COMMA)   , (';',SEMI), ('_',UNDERSCORE)
             , ('\\',LAMBDA) , ('\x03BB',LAMBDA) ]
    <|> reserveds [ ("True",TRUE), ("False",FALSE)
                  , ("if",IF), ("then",THEN), ("else",ELSE)
                  , ("case",CASE), ("of",OF), ("data", DATA) 
                  , ("let",LET), ("in",IN) ]
    <|> do { char '`'; v <- variable; char '`'; return $ OP v }
    <|> anyOp
    <|> do { char '"'; s <- many $ backslashed <|> satisfy (/='"'); char '"'
           ; return $ STRING s}
    <|> do { char '\''; c <- backslashed <|> satisfy (/='\''); char '\''
           ; return $ CHAR c}
    <|> (ID <$> variable)
    <|> typeVar
    <|> do { try $ string "\r\n" <|> string "\n"; return NEWLINE }

chrs :: [(Char, Token)] -> Parsec String u Token
chrs = choice . map chr
    where chr (c, t) = char c >> return t

reserveds :: [(String, Token)] -> Parsec String u Token
reserveds = choice . map reserved
    where reserved (s, t) = try $ string s >>
                                  notFollowedBy (alphaNum <|> char '_') >>
                                  return t

anyOp = do op <- many1 (satisfy isSymbol <|> oneOf "+-/*=.$<>:&|^?%#@~!")
           case op of { ".." -> return DOT2
                      ; "->" -> return ARROW; "\8594" -> return ARROW
                      ; _ -> return $ OP op }

backslashed :: Parsec String u Char
backslashed = do { char '\\'; c <- anyChar
                 ; return . read $ ['\'','\\',c,'\''] }

integer :: Parsec String u Int
integer = read <$> many1 digit

variable :: Parsec String u String
variable = identifier $ letter <|> char '_'

typeVar :: Parsec String u Token
typeVar = TYPE <$> identifier upper

identifier :: Parsec String u Char -> Parsec String u String
identifier c = (:) <$> c <*> (many $ alphaNum <|> oneOf "_\'")

----  White Space and comments  ----

whitespace = (lineComment >> return NEWLINE) <|>
             (many1 (multiComment <|> many1 (char ' ')) >> return SPACES)
lineComment = do try $ string "--"
                 manyTill anyChar $ newline <|> (eof >> return '\n')

multiComment = do { try $ string "{-"; closeComment }
closeComment = manyTill anyChar . choice $
               [ try $ string "-}"
               , do { try $ string "{-"; closeComment; closeComment }
               ]

tokenParser :: Parsec String u [Token]
tokenParser = many1 token

tokenize :: String -> Either String [Token]
tokenize s = case parse tokenParser "" s of
               Right ts -> Right ts
               Left err -> Left $ "Syntax error: " ++ show err
