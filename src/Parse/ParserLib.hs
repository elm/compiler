module ParserLib where

import Ast
import Combinators
import Data.Char (isUpper)
import Control.Monad (liftM)
import Tokens

varNoSpace = do { t <- item; case t of { ID v -> return v; _ -> zero } }
var = whitespace >> varNoSpace
capVar = do
  whitespace; t <- item
  case t of { ID (v:vs) -> if isUpper v then return (v:vs) else zero }

chr = do { whitespace; t <- item; case t of { CHAR c -> return c; _ -> zero } }

tNoSpace token = sat (==token)
tWithSpace token = forcedWS >> sat (==token)
t token = whitespace >> sat (==token)

forcedWS = do { sat (==SPACES); star nl_space } +|+ plus nl_space
    where nl_space = plus (sat (==NEWLINE)) >> sat (==SPACES)
whitespace = optional forcedWS

accessible exp = do
  e <- exp
  access <- optional $ opParserNospace (==".")
  case access of
    Just "." -> accessible (Access e `liftM` var)
    Nothing -> return e

opParserNospace pred =
    do { t <- item
       ; case t of { OP op -> if pred op then return op else zero; _ -> zero } }
opParser pred = whitespace >> opParserNospace pred
anyOp  = opParser (const True)
assign = opParser (=="=")
