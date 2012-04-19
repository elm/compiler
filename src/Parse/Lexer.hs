
module Lexer (tokenize) where

import Data.Char (isSymbol)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim (parse,(<|>),many,try)
import Tokens

token = do { integer >>= return . NUMBER }
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
    <|> do { variable >>= return . ID }
    <|> typeVar
    <|> do { try $ string "\r\n" <|> string "\n"; return NEWLINE }

str s t = do { try $ string s; return t }
chrs = choice . map (uncurry chr)
chr c t = do { char c; return t }
reserveds = choice . map (uncurry reserved)
reserved str token =
    try $ do {string str; notFollowedBy $ alphaNum <|> char '_'; return token }
anyOp = do op <- many1 (satisfy isSymbol <|> oneOf "+-/*=.$<>:&|^?%#@~!")
           case op of { ".." -> return DOT2
                      ; "->" -> return ARROW; "\8594" -> return ARROW
                      ; _ -> return $ OP op }

backslashed = do { char '\\'; c <- satisfy (\x -> True)
                 ; return . read $ ['\'','\\',c,'\''] }

integer = return . read =<< many1 digit
variable = do
  shd <- letter <|> char '_'
  stl <- many $ alphaNum <|> char '_' <|> char '\''
  return $ shd:stl
typeVar = do
  shd <- upper
  stl <- many $ alphaNum <|> char '_' <|> char '\''
  return . TYPE $ shd:stl


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

token_parser = many1 token

tokenize s = case parse token_parser "" s of
               Right ts -> Right ts
               Left err -> Left $ "Syntax error: " ++ show err
