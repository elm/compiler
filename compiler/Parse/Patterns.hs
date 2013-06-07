
module Parse.Patterns (patternTerm, patternExpr, makeLambda, flattenPatterns) where

import Ast
import Located
import Control.Applicative ((<$>),(<*>),(*>),pure)
import Control.Monad
import Control.Monad.State
import Data.Char (isUpper)
import Guid
import Text.Parsec hiding (newline,spaces,State)
import Text.Parsec.Indent
import Parse.Library

patternBasic :: IParser Pattern
patternBasic =
    choice [ char '_' >> return PAnything
           , do x@(c:_) <- var
                return $ if isUpper c then PData x [] else PVar x
           ]

asPattern :: IParser Pattern -> IParser Pattern
asPattern pattern = do
  p <- pattern
  var <- optionMaybe (try (whitespace >> reserved "as" >> whitespace >> lowVar))
  return $ case var of
             Just v -> PAsVar v p
             Nothing -> p

patternRecord :: IParser Pattern
patternRecord = PRecord <$> brackets (commaSep1 lowVar)

patternTuple :: IParser Pattern
patternTuple = do ps <- parens (commaSep patternExpr)
                  return $ case ps of { [p] -> p; _ -> ptuple ps }

patternList :: IParser Pattern
patternList = plist <$> braces (commaSep patternExpr)

patternTerm :: IParser Pattern
patternTerm = (choice [ patternRecord, patternTuple, patternList,
                        patternBasic ])
              <?> "pattern"

patternConstructor :: IParser Pattern
patternConstructor = PData <$> capVar <*> spacePrefix patternTerm

patternExpr :: IParser Pattern
patternExpr = 
    asPattern (foldr1 pcons <$> consSep1 (patternConstructor <|> patternTerm))
    <?> "pattern"

makeLambda :: [Pattern] -> CExpr -> GuidCounter CExpr
makeLambda pats body = go (reverse pats) body
    where go [] body = return body
          go (p:ps) body@(L t s _) = do
            (x,e) <- extract p body
            go ps (L t s $ Lambda x e)
          
extract :: Pattern -> CExpr -> GuidCounter (String, CExpr)
extract pattern body@(L t s _) =
  let loc = L t s in
  let fn x e = (x,e) in
  case pattern of
    PAnything -> return $ fn "_" body
    PVar x -> return $ fn x body
    PAsVar x PAnything -> return $ fn x body
    PAsVar x p -> do
      (x', body') <- extract p body
      return $ fn x (loc $ Let [FnDef x' [] (loc $ Var x)] body')
    PData name ps -> do
        x <- guid
        let a = '_' : show x
        return . fn a . loc $ Case (loc (Var a)) [(pattern, body)]
    PRecord fs -> do
        x <- guid
        let a = '_' : show x
            toDef f = FnDef f [] (loc $ Access (loc $ Var a) f)
        return . fn a . loc $ Let (map toDef fs) body

extracts :: [Pattern] -> CExpr -> GuidCounter ([String], CExpr)
extracts ps body = go [] (reverse ps) body
    where go args [] body = return (args, body)
          go args (p:ps) body = do (x,e) <- extract p body
                                   go (x:args) ps e

flattenPatterns :: [Pattern] -> CExpr -> GuidCounter (IParser [Def])
flattenPatterns patterns exp@(L t s _) =
  let loc = L t s in
  case patterns of
    PVar f : args -> do
        (as,e) <- extracts args exp
        return . return $
               if isOp (head f) then let [a,b] = as in [ OpDef f a b e ]
                                else [ FnDef f as e ]

    [p] -> return `liftM` matchSingle p exp p

    _ -> return . fail $ "Pattern (" ++ unwords (map show patterns) ++
                ") cannot be used on the left-hand side of an assign statement."

matchSingle :: Pattern -> CExpr -> Pattern -> GuidCounter [Def]
matchSingle pat exp@(L t s _) p =
  let loc = L t s in
  case p of
    PData _ ps -> do
        x <- guid
        let v = '_' : show x
        dss <- mapM (matchSingle pat . loc $ Var v) ps
        return (FnDef v [] exp : concat dss)

    PVar x ->
        return [ FnDef x [] (loc $ Case exp [(pat, loc $ Var x)]) ]

    PAsVar x p' -> do
        subPat <- matchSingle p' (loc $ Var x) p'
        return $ (FnDef x [] (loc $ Case exp [(pat, loc $ Var x)])):subPat
      
    PRecord fs -> do
        a <- (\x -> '_' : show x) `liftM` guid
        let toDef f = FnDef f [] (loc $ Access (loc $ Var a) f)
        return (FnDef a [] exp : map toDef fs)

    PAnything -> return []
