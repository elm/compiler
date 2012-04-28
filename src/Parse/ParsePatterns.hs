module ParsePatterns (patternTerm, patternExpr) where

import Ast
import Combinators
import Data.Char (isUpper)
import ParserLib
import Tokens
import Control.Monad (liftM)

patternBasic =
    (t UNDERSCORE >> return PAnything) +|+
    (do { x@(c:_) <- var ;
          if isUpper c then PData x `liftM` star patternTerm
                       else return $ PVar x })

patternTuple = do
  t LPAREN; ps <- sepBy1 (t COMMA) patternExpr; t RPAREN
  return $ case ps of { [p] -> p; _ -> ptuple ps }

patternCons = do
  p <- patternTerm
  colon <- optional $ opParser (==":")
  case colon of
    Just ":" -> pcons p `liftM` patternExpr
    Nothing -> return p

patternList = do
  t LBRACKET; ps <- sepBy (t COMMA) patternExpr; t RBRACKET; return (plist ps)

patternTerm = patternTuple +|+ patternList +|+ patternBasic
patternExpr = patternTuple +|+ patternList +|+ patternCons
