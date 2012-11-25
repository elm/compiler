module Parse.Patterns (patternTerm, patternExpr, makeLambda, flattenPatterns) where

import Ast
import Data.Char (isUpper)
import Control.Applicative ((<$>),(<*>))
import Control.Monad
import Control.Monad.State
import Text.Parsec hiding (newline,spaces,State)
import Text.Parsec.Indent
import Parse.Library

patternBasic :: IParser Pattern
patternBasic =
    choice [ char '_' >> return PAnything
           , do x@(c:_) <- var
                return $ if isUpper c then PData x [] else PVar x
           ]

patternTuple :: IParser Pattern
patternTuple = do ps <- parens (commaSep patternExpr)
                  return $ case ps of { [p] -> p; _ -> ptuple ps }

patternList :: IParser Pattern
patternList = plist <$> braces (commaSep patternExpr)

patternTerm :: IParser Pattern
patternTerm = patternTuple <|> patternList <|> patternBasic <?> "pattern"

patternConstructor :: IParser Pattern
patternConstructor = PData <$> capVar <*> spacePrefix patternTerm

patternExpr :: IParser Pattern
patternExpr = foldr1 pcons <$> consSep1 (patternConstructor <|> patternTerm) <?> "pattern"


makeLambda pats body = foldr Lambda (makeBody pats body) (map getName pats)

makeBody pats body = foldr func body pats
    where func PAnything e = e
          func (PVar x)  e = e
          func p e = Case (Var $ getName p) [(p,e)]

flattenPatterns (PVar f : args) exp
    | isOp (head f) = let [a,b] = (map getName args) in
                      return [ OpDef f a b (makeBody args exp) ]
    | otherwise     = return [ FnDef f (map getName args) (makeBody args exp) ]


flattenPatterns [p] exp = return $ matchSingle p exp p

flattenPatterns ps _ = 
    fail $ "Pattern (" ++ unwords (map show ps) ++
           ") cannot be used on the left-hand side of an assign statement."

matchSingle pat exp p@(PData _ ps) =
    (FnDef v [] exp) : concatMap (matchSingle p $ Var v) ps
        where v = getName p
matchSingle pat exp (PVar x)  = [ FnDef x [] (Case exp [(pat,Var x)]) ]
matchSingle pat exp PAnything = []

getName p = f p
    where f (PData n ps) = n ++ "$" ++ concatMap getName ps
          f (PAnything)  = "_"
          f (PVar x)     = x

