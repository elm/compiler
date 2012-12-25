module Parse.Patterns (patternTerm, patternExpr, makeLambda, flattenPatterns) where

import Ast
import Context
import Control.Applicative ((<$>),(<*>))
import Control.Monad
import Control.Monad.State
import Data.Char (isUpper)
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

makeLambda :: [Pattern] -> CExpr -> CExpr
makeLambda pats body =
    foldr (\x e -> noContext $ Lambda x e)
          (makeBody pats body)
          (map getName pats)

makeBody pats body = foldr func body pats
    where func PAnything e = e
          func (PVar x)  e = e
          func p (C t s e) = C t s $ Case (C t s . Var $ getName p) [(p,C t s e)]

flattenPatterns :: [Pattern] -> CExpr -> IParser [Def]
flattenPatterns (PVar f : args) exp
    | isOp (head f) = let [a,b] = (map getName args) in
                      return [ OpDef f a b (makeBody args exp) ]
    | otherwise     = return [ FnDef f (map getName args) (makeBody args exp) ]


flattenPatterns [p] exp = return $ matchSingle p exp p
flattenPatterns ps _ = 
    fail $ "Pattern (" ++ unwords (map show ps) ++
           ") cannot be used on the left-hand side of an assign statement."

matchSingle :: Pattern -> CExpr -> Pattern -> [Def]
matchSingle pat exp p@(PData _ ps) =
    (FnDef v [] exp) : concatMap (matchSingle p $ noContext $ Var v) ps
        where v = getName p
matchSingle pat exp (PVar x) =
    [ FnDef x [] (noContext $ Case exp [(pat, noContext $ Var x)]) ]
matchSingle pat exp PAnything = []

getName :: Pattern -> String
getName p = case p of
              PData n ps -> n ++ "$" ++ concatMap getName ps
              PAnything  -> "_"
              PVar x     -> x