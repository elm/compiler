module Patterns (patternTerm, patternExpr, makeFunction) where

import Ast
import Data.Char (isUpper)
import Control.Applicative ((<$>))
import Control.Monad
import Text.Parsec
import ParseLib

makeFunction args body = foldr func body args
    where func PAnything e = Lambda "_" e
          func (PVar x)  e = Lambda x e
          func p e = "t" `Lambda` Case (Var "t") [(p,e)]

patternBasic :: Monad m => ParsecT [Char] u m Pattern
patternBasic =
    choice [ char '_' >> return PAnything
           , do x@(c:_) <- var
                if isUpper c then PData x <$> spacePrefix patternTerm
                             else return $ PVar x
           ]

patternTuple :: Monad m => ParsecT [Char] u m Pattern
patternTuple = do ps <- parens (commaSep patternExpr)
                  return $ case ps of { [p] -> p; _ -> ptuple ps }

patternList :: Monad m => ParsecT [Char] u m Pattern
patternList = plist <$> braces (commaSep patternExpr)

patternTerm :: Monad m => ParsecT [Char] u m Pattern
patternTerm = patternTuple <|> patternList <|> patternBasic <?> "pattern"

patternExpr :: Monad m => ParsecT [Char] u m Pattern
patternExpr = foldl1 pcons <$> consSep1 patternTerm <?> "pattern"
