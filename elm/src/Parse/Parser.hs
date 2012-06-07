module Parser where

import Ast
import Binop (binops)
import Combinators
import Control.Monad (liftM)
import Data.List (foldl')
import Guid
import Lexer
import ParserLib
import ParseTypes (datatype)
import ParsePatterns
import Tokens
import Types (Type (VarT))

--------  Basic Terms  --------

numTerm = do { whitespace; t <- item
             ; case t of { NUMBER n -> return $ Number n; _ -> zero } }
strTerm = do { whitespace; t <- item
             ; case t of { STRING cs -> return $ Str cs
                         ; _ -> zero } }

varTerm = Var `liftM` var
chrTerm = Chr `liftM` chr

trueTerm = do { t TRUE; return $ Boolean True }
falseTerm = do { t FALSE; return $ Boolean False }


--------  Complex Terms  --------

listTerm = (do { t LBRACKET; start <- expr; t DOT2; end <- expr; t RBRACKET
                ; return $ Range start end }) +|+
           (do { t LBRACKET; es <- sepBy (t COMMA) expr; t RBRACKET
               ; return $ list es })

parensTerm = (do { t LPAREN; op <- anyOp; t RPAREN
                  ; return . Lambda "x" . Lambda "y" $
                           Binop op (Var "x") (Var "y") }) +|+
             (do { t LPAREN; es <- sepBy (t COMMA) expr; t RPAREN
                 ; return $ case es of { [e] -> e; _ -> tuple es } })

term = select [ numTerm
              , strTerm
              , accessible varTerm
              , chrTerm
              , trueTerm
              , falseTerm
              , listTerm 
              , accessible parensTerm
              ]

--------  Applications  --------

appExpr = do
  tlist <- plus term
  return $ case tlist of
             t:[] -> t
             t:ts -> foldl' App t ts


--------  Expressions with infix operators  --------

binaryExpr = binops appExpr anyOp


--------  Normal Expressions  --------

ifExpr = do { t IF; e1 <- expr; t THEN; e2 <- expr; t ELSE; e3 <- expr
             ; return $ If e1 e2 e3 }

lambdaExpr = do { t LAMBDA; vs <- plus var; t ARROW; e <- expr
                 ; return $ foldr Lambda e vs }

assignExpr = whitespace >> assignExprNospace
assignExprNospace = do
  p:ps <- plus patternTerm; assign; e <- expr
  case p:ps of
    PVar x : _ -> return (x, foldr func e ps)
        where func PAnything e' = Lambda "_" e'
              func (PVar x)  e' = Lambda x e'
              func p' e' = Lambda "_temp" (Case (Var "_temp") [(p', e')])
--    _ : [] -> return $ \hole -> Case e [(p,hole)]
    _ -> zero

letExpr = do
  t LET; brace <- optional $ t LBRACE
  case brace of
    Nothing -> do f <- assignExpr; t IN; e <- expr; return (Let [f] e)
    Just LBRACE -> do fs <- sepBy1 (t SEMI) assignExpr; t RBRACE; t IN;
                      e <- expr; return (Let fs e)

caseExpr = do
  t CASE; e <- expr; t OF; t LBRACE
  cases <- sepBy1 (t SEMI)
           (do { p <- patternExpr; t ARROW; e <- expr; return (p,e) })
  t RBRACE
  return $ Case e cases

--------  All Expressions  --------

expr = select [ letExpr
              , binaryExpr
              , ifExpr
              , caseExpr
              , lambdaExpr
              ]

def = do (f,e) <- assignExprNospace
         return ([f], [e], guid >>= \x -> return [VarT x])

defs = do
  (fss,ess,tss) <- unzip3 `liftM` plus (whitespace >> plus (sat (==NEWLINE)) >> def +|+ datatype)
  let (fs,es,ts) = (concat fss, concat ess, concat `liftM` sequence tss)
  star $ sat (==NEWLINE) +|+ sat (==SPACES)
  return (Let (zip fs es) (Var "main"), liftM (zip fs) ts)

err = "Parse Error: Better error messages to come!"
toExpr = extractResult err . parse expr
toDefs = extractResult err . parse defs . (NEWLINE:)
