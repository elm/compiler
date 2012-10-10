module Patterns (patternTerm, patternExpr, makeLambda, flattenPatterns) where

import Ast
import Data.Char (isUpper)
import Control.Applicative ((<$>),(<*>))
import Control.Monad
import Text.Parsec
import ParseLib

patternBasic :: Monad m => ParsecT [Char] u m Pattern
patternBasic =
    choice [ char '_' >> return PAnything
           , do x@(c:_) <- var
                return $ if isUpper c then PData x [] else PVar x
           ]

patternTuple :: Monad m => ParsecT [Char] u m Pattern
patternTuple = do ps <- parens (commaSep patternExpr)
                  return $ case ps of { [p] -> p; _ -> ptuple ps }

patternList :: Monad m => ParsecT [Char] u m Pattern
patternList = plist <$> braces (commaSep patternExpr)

patternTerm :: Monad m => ParsecT [Char] u m Pattern
patternTerm = patternTuple <|> patternList <|> patternBasic <?> "pattern"

patternConstructor :: Monad m => ParsecT [Char] u m Pattern
patternConstructor = PData <$> capVar <*> spacePrefix patternTerm

patternExpr :: Monad m => ParsecT [Char] u m Pattern
patternExpr = foldr1 pcons <$> consSep1 (patternConstructor <|> patternTerm) <?> "pattern"


makeLambda pats body = foldr Lambda (makeBody pats body) (map getName pats)

makeBody pats body = foldr func body pats
    where func PAnything e = e
          func (PVar x)  e = e
          func p e = Case (Var $ getName p) [(p,e)]

flattenPatterns (PVar f : args) exp =
    return [ Definition f (map getName args) (makeBody args exp) ]
flattenPatterns [p] exp = return $ matchSingle p exp p
flattenPatterns ps _ = 
    fail $ "Pattern (" ++ unwords (map show ps) ++
           ") cannot be used on the left-hand side of an assign statement."

matchSingle pat exp p@(PData _ ps) =
    (Definition v [] exp) : concatMap (matchSingle p $ Var v) ps
        where v = getName p
matchSingle pat exp (PVar x)  = [ Definition x [] (Case exp [(pat,Var x)]) ]
matchSingle pat exp PAnything = []

getName p = f p
    where f (PData n ps) = n ++ "$" ++ concatMap getName ps
          f (PAnything)  = "_"
          f (PVar x)     = x

