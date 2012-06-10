module Patterns (patternTerm, patternExpr, makeFunction, flattenPatterns) where

import Ast
import Data.Char (isUpper)
import Control.Applicative ((<$>))
import Control.Monad
import Text.Parsec
import ParseLib

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



makeFunction args body = foldr func body args
    where func PAnything e = Lambda "_" e
          func (PVar x)  e = Lambda x e
          func p e = "t" `Lambda` Case (Var "t") [(p,e)]

flattenPatterns (PVar f : args) exp = return [(f, makeFunction args exp)]
flattenPatterns [p] exp = return $ matchSingle p exp p
flattenPatterns ps _ = 
    fail $ "Pattern (" ++ unwords (map show ps) ++
           ") cannot be used on the left-hand side of an assign statement."

matchSingle pat exp p@(PData _ ps) =
    (v, exp) : concatMap (matchSingle p $ Var v) ps
        where v = "'" ++ getName p
matchSingle pat exp (PVar x)  = [ (x, Case exp [(pat,Var x)]) ]
matchSingle pat exp PAnything = []

getName (PData n ps) = n ++ concatMap getName ps
getName (PAnything)  = "_"
getName (PVar x)     = x

