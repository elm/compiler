module ParserLib where

import Ast
import Combinators
import Data.Char (isUpper)
import Tokens

var_noSpace = do { t <- item; case t of { ID v -> return v; _ -> zero } }
var = whitespace >> var_noSpace
cap_var = do
  whitespace; t <- item
  case t of { ID (v:vs) -> if isUpper v then return (v:vs) else zero }

chr = do { whitespace; t <- item; case t of { CHAR c -> return c; _ -> zero } }

t_noSpace token = sat (==token)
t_withSpace token = forcedWS >> sat (==token)
t token = whitespace >> sat (==token)

forcedWS = do { sat (==SPACES); star nl_space } +|+ plus nl_space
    where nl_space = plus (sat (==NEWLINE)) >> sat (==SPACES)
whitespace = optional forcedWS

accessible exp = do
  e <- exp
  access <- optional $ op_parser_nospace (==".")
  case access of
    Just "." -> accessible (var >>= return . Access e)
    Nothing -> return e

op_parser_nospace pred =
    do { t <- item
       ; case t of { OP op -> if pred op then return op else zero; _ -> zero } }
op_parser pred = whitespace >> op_parser_nospace pred
anyOp  = op_parser (const True)
assign = op_parser (=="=")