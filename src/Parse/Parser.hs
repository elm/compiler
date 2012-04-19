module Parser where

import Ast
import Binop (binops)
import Combinators
import Data.List (foldl')
import Lexer
import ParserLib
import ParseTypes (datatype)
import ParsePatterns
import Tokens

--------  Basic Terms  --------

num_term = do { whitespace; t <- item
              ; case t of { NUMBER n -> return $ Number n; _ -> zero } }
str_term = do { whitespace; t <- item
              ; case t of { STRING cs -> return . list $ map Chr cs
                          ; _ -> zero } }

var_term = do var >>= return . Var
chr_term = do chr >>= return . Chr

true_term = do { t TRUE; return $ Boolean True }
false_term = do { t FALSE; return $ Boolean False }


--------  Complex Terms  --------

list_term = (do { t LBRACKET; start <- expr; t DOT2; end <- expr; t RBRACKET
                ; return $ Range start end }) +|+
            (do { t LBRACKET; es <- sepBy (t COMMA) expr; t RBRACKET
                ; return $ list es })

parens_term = (do { t LPAREN; op <- anyOp; t RPAREN
                  ; return . Lambda "x" . Lambda "y" $
                           Binop op (Var "x") (Var "y") }) +|+
              (do { t LPAREN; es <- sepBy (t COMMA) expr; t RPAREN
                  ; return $ case es of { [e] -> e; _ -> tuple es } })

term = select [ num_term
              , str_term
              , accessible var_term
              , chr_term
              , true_term
              , false_term
              , list_term 
              , accessible parens_term
              ]

--------  Applications  --------

app_expr = do
  tlist <- plus term
  return $ case tlist of
             t:[] -> t
             t:ts -> foldl' App t ts


--------  Expressions with infix operators  --------

binary_expr = binops app_expr anyOp


--------  Normal Expressions  --------

if_expr = do { t IF; e1 <- expr; t THEN; e2 <- expr; t ELSE; e3 <- expr
             ; return $ If e1 e2 e3 }

lambda_expr = do { t LAMBDA; vs <- plus var; t ARROW; e <- expr
                 ; return $ foldr (\x e -> Lambda x e) e vs }

assign_expr = whitespace >> assign_expr_nospace
assign_expr_nospace = do
  p:ps <- plus pattern_term; assign; e <- expr
  case p:ps of
    PVar x : _ -> return (x, foldr func e ps)
        where func PAnything e' = Lambda "_" e'
              func (PVar x)  e' = Lambda x e'
              func p' e' = Lambda "_temp" (Case (Var "_temp") [(p', e')])
--    _ : [] -> return $ \hole -> Case e [(p,hole)]
    _ -> zero

let_expr = do
  t LET; brace <- optional $ t LBRACE
  case brace of
    Nothing -> do f <- assign_expr; t IN; e <- expr; return (Let [f] e)
    Just LBRACE -> do fs <- sepBy1 (t SEMI) assign_expr; t RBRACE; t IN;
                      e <- expr; return (Let fs e)

case_expr = do
  t CASE; e <- expr; t OF; t LBRACE
  cases <- sepBy1 (t SEMI)
           (do { p <- pattern_expr; t ARROW; e <- expr; return (p,e) })
  t RBRACE
  return $ Case e cases

--------  All Expressions  --------

expr = select [ let_expr
              , binary_expr
              , if_expr
              , case_expr
              , lambda_expr
              ]

def = assign_expr_nospace >>= return . (:[])

defs = do
  ds <- plus (whitespace >> plus (sat (==NEWLINE)) >> def +|+ datatype)
  star $ sat (==NEWLINE) +|+ sat (==SPACES)
  return $ Let (concat ds) (Var "main")

err = "Parse Error: Better error messages to come!"
toExpr = extractResult err . parse expr
toDefs = extractResult err . parse defs . (NEWLINE:)
