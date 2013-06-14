{-# LANGUAGE DeriveDataTypeable #-}
module SourceSyntax.Expression where

import Data.Data
import Data.List (intercalate)
import qualified Text.Pandoc as Pandoc
import qualified SourceSyntax.Helpers as Help
import qualified SourceSyntax.Location as Location
import qualified SourceSyntax.Pattern as Pattern
import qualified SourceSyntax.Literal as Literal
import Types.Types

type LExpr = Location.Located Expr
data Expr = Literal Literal.Literal
          | Range LExpr LExpr
          | Access LExpr String
          | Remove LExpr String
          | Insert LExpr String LExpr
          | Modify LExpr [(String,LExpr)]
          | Record [(String,[String],LExpr)]
          | Binop String LExpr LExpr
          | Lambda String LExpr
          | App LExpr LExpr
          | MultiIf [(LExpr,LExpr)]
          | Let [Def] LExpr
          | Var String
          | Case LExpr [(Pattern.Pattern,LExpr)]
          | Data String [LExpr]
          | Markdown Pandoc.Pandoc
            deriving (Eq, Data, Typeable)

data Def = FnDef String [String] LExpr
         | OpDef String String String LExpr
         | TypeAnnotation String Type
           deriving (Eq, Data, Typeable)

cons h t = Location.merge h t (Data "Cons" [h,t])
nil      = Location.L (Just "[]") Location.NoSpan (Data "Nil" [])
list     = foldr cons nil
tuple es = Data ("Tuple" ++ show (length es)) es

delist (Location.L _ _ (Data "Cons" [h,t])) = h : delist t
delist _ = []


instance Show Expr where
  show e =
   let show' (Location.L _ _ e) = Help.parensIf (needsParens e) (show e) in
   case e of
     Literal lit -> show lit
     Range e1 e2 -> "[" ++ show e1 ++ ".." ++ show e2 ++ "]"
     Access e x -> show' e ++ "." ++ x
     Remove e x -> Help.brkt (show e ++ " - " ++ x)
     Insert (Location.L _ _ (Remove e y)) x v ->
         Help.brkt (show e ++ " - " ++ y ++ " | " ++ x ++ " = " ++ show v)
     Insert e x v -> Help.brkt (show e ++ " | " ++ x ++ " = " ++ show v)
     Modify e fs -> Help.brkt (show e ++" | "++ intercalate ", " (map field fs))
         where field (x,e) = x ++ " <- " ++ show e
     Record r -> Help.brkt (intercalate ", " (map fields r))
         where fields (f,args,e) = f ++ concatMap (' ':) args ++ " = " ++ show e
     Binop op e1 e2 -> show' e1 ++ " " ++ op ++ " " ++ show' e2
     Lambda x e -> let (xs,e') = getLambdas (Location.none $ Lambda x e) in
                      concat [ "\\", intercalate " " xs, " -> ", show e' ]
     App e1 e2 -> show' e1 ++ " " ++ show' e2
     MultiIf (p:ps) -> concat [ "if | ", iff p, sep (map iff ps) ]
         where iff (b,e) = show b ++ " -> " ++ show e
               sep = concatMap ("\n   | " ++)
     Let defs e -> "let { "++intercalate " ; " (map show defs)++" } in "++show e
     Var (c:cs) -> if Help.isOp c then Help.parens (c:cs) else c:cs
     Case e pats -> "case "++ show e ++" of " ++ Help.brkt (intercalate " ; " pats')
         where pats' = map (\(p,e) -> show p ++ " -> " ++ show e) pats
     Data name es
          | name == "Cons" -> ("["++) . (++"]") . intercalate "," . map show $
                              delist (Location.none $ Data "Cons" es)
          | name == "Nil"  -> "[]"
          | otherwise      -> name ++ " " ++ intercalate " " (map show' es)
     Markdown _ -> "[markdown| ... |]"


instance Show Def where
  show e =
   case e of
     FnDef v [] e     -> v ++ " = " ++ show e
     FnDef f args e   -> f ++ concatMap (' ':) args ++ " = " ++ show e
     OpDef op a1 a2 e -> intercalate " " [a1,op,a2] ++ " = " ++ show e
     TypeAnnotation n t -> n ++ " : " ++ show t

getLambdas (Location.L _ _ (Lambda x e)) = (x:xs,e')
    where (xs,e') = getLambdas e
getLambdas e = ([],e)

needsParens e =
  case e of
    Binop _ _ _ -> True
    Lambda _ _  -> True
    App _ _     -> True
    MultiIf _   -> True
    Let _ _     -> True
    Case _ _    -> True
    Data name (x:xs) -> name /= "Cons"
    _ -> False
