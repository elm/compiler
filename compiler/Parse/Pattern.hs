
module Parse.Pattern (term, expr, makeLambda, flatten) where

import Control.Applicative ((<$>),(<*>),pure)
import Control.Monad
import Control.Monad.State
import Data.Char (isUpper)
import Unique
import Text.Parsec hiding (newline,spaces,State)
import Text.Parsec.Indent

import Parse.Helpers
import Parse.Literal
import qualified SourceSyntax.Pattern as Pattern
import SourceSyntax.Everything hiding (parens, tuple)

basic :: IParser Pattern
basic = choice
    [ char '_' >> return PAnything
    , do v <- var
         return $ case v of
                    "True"  -> PLiteral (Boolean True)
                    "False" -> PLiteral (Boolean False)
                    c : _   -> if isUpper c then PData v [] else PVar v
    , do lit <- literal
         return $ case lit of
                    Str s -> foldr combine (PData "[]" []) s
                       where combine h t = PData "::" [PLiteral (Chr h),t]
                    _ -> PLiteral lit
    ]

asPattern :: Pattern -> IParser Pattern
asPattern pattern = do
  var <- optionMaybe (try (whitespace >> reserved "as" >> whitespace >> lowVar))
  return $ case var of
             Just v -> PAlias v pattern
             Nothing -> pattern

record :: IParser Pattern
record = PRecord <$> brackets (commaSep1 lowVar)

tuple :: IParser Pattern
tuple = do ps <- parens (commaSep expr)
           return $ case ps of { [p] -> p; _ -> Pattern.tuple ps }

list :: IParser Pattern
list = Pattern.list <$> braces (commaSep expr)

term :: IParser Pattern
term =
     (choice [ record, tuple, list, basic ]) <?> "pattern"

patternConstructor :: IParser Pattern
patternConstructor = do
  v <- capVar
  case v of
    "True"  -> return $ PLiteral (Boolean True)
    "False" -> return $ PLiteral (Boolean False)
    _       -> PData v <$> spacePrefix term

expr :: IParser Pattern
expr = do
  patterns <- consSep1 (patternConstructor <|> term)
  asPattern (foldr1 Pattern.cons patterns) <?> "pattern"

makeLambda :: [Pattern] -> LExpr t v -> Unique (LExpr t v)
makeLambda pats body = go (reverse pats) body
    where go [] body = return body
          go (p:ps) body@(L t s _) = do
            (x,e) <- extract p body
            go ps (L t s $ Lambda x e)
          
extract :: Pattern -> LExpr t v -> Unique (String, LExpr t v)
extract pattern body@(L t s _) =
  let loc = L t s in
  let fn x e = (x,e) in
  case pattern of
    PAnything -> return $ fn "_" body
    PVar x -> return $ fn x body
    PAlias x PAnything -> return $ fn x body
    PAlias x p -> do
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

extracts :: [Pattern] -> LExpr t v -> Unique ([String], LExpr t v)
extracts ps body = go [] (reverse ps) body
    where go args [] body = return (args, body)
          go args (p:ps) body = do (x,e) <- extract p body
                                   go (x:args) ps e

flatten :: [Pattern] -> LExpr t v -> Unique (IParser [Def t v])
flatten patterns exp@(L t s _) =
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

matchSingle :: Pattern -> LExpr t v -> Pattern -> Unique [Def t v]
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

    PAlias x p' -> do
        subPat <- matchSingle p' (loc $ Var x) p'
        return $ (FnDef x [] (loc $ Case exp [(pat, loc $ Var x)])):subPat
      
    PRecord fs -> do
        a <- (\x -> '_' : show x) `liftM` guid
        let toDef f = FnDef f [] (loc $ Access (loc $ Var a) f)
        return (FnDef a [] exp : map toDef fs)

    PAnything -> return []
