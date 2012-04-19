module ParsePatterns (pattern_term, pattern_expr) where

import Ast
import Combinators
import Data.Char (isUpper)
import ParserLib
import Tokens
import Types

pattern_basic =
    (t UNDERSCORE >> return PAnything) +|+
    (do { x@(c:_) <- var ;
          if isUpper c then star pattern_term >>= return . PData x
                       else return $ PVar x })

pattern_tuple = do
  t LPAREN; ps <- sepBy1 (t COMMA) pattern_expr; t RPAREN
  return $ case ps of { [p] -> p; _ -> ptuple ps }

pattern_cons = do
  p <- pattern_term
  colon <- optional $ op_parser (==":")
  case colon of
    Just ":" -> pattern_expr >>= return . pcons p
    Nothing -> return p

pattern_list = do
  t LBRACKET; ps <- sepBy (t COMMA) pattern_expr; t RBRACKET; return (plist ps)

pattern_term = pattern_tuple +|+ pattern_list +|+ pattern_basic
pattern_expr = pattern_tuple +|+ pattern_list +|+ pattern_cons
