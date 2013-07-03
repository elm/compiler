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

type LExpr tipe var = Location.Located (Expr tipe var)
data Expr t v
    = Literal Literal.Literal
    | Var String
    | Range (LExpr t v) (LExpr t v)
    | ExplicitList [LExpr t v]
    | Binop String (LExpr t v) (LExpr t v)
    | Lambda Pattern.Pattern (LExpr t v)
    | App (LExpr t v) (LExpr t v)
    | MultiIf [(LExpr t v,LExpr t v)]
    | Let [Def t v] (LExpr t v)
    | Case (LExpr t v) [(Pattern.Pattern, LExpr t v)]
    | Data String [LExpr t v]
    | Access (LExpr t v) String
    | Remove (LExpr t v) String
    | Insert (LExpr t v) String (LExpr t v)
    | Modify (LExpr t v) [(String, LExpr t v)]
    | Record [(String, [String], LExpr t v)] -- [Def t v]
    | Markdown Pandoc.Pandoc
      deriving (Eq, Data, Typeable)

data Def tipe var
    = Def String [String] (LExpr tipe var)
    | TypeAnnotation String Type
      deriving (Eq, Data, Typeable)

tuple es = Data ("Tuple" ++ show (length es)) es

delist (Location.L _ _ (Data "::" [h,t])) = h : delist t
delist _ = []


instance Show (Expr t v) where
  show e =
   let show' (Location.L _ _ e) = Help.parensIf (needsParens e) (show e) in
   case e of
     Literal lit -> show lit
     Range e1 e2 -> "[" ++ show e1 ++ ".." ++ show e2 ++ "]"
     ExplicitList es -> "[" ++ intercalate "," (map show es) ++ "]"
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
     Data "::" [h,t] -> show h ++ " :: " ++ show t
     Data "[]" [] -> "[]"
     Data name es -> name ++ " " ++ intercalate " " (map show' es)
     Access e x -> show' e ++ "." ++ x
     Remove e x -> Help.brkt (show e ++ " - " ++ x)
     Insert (Location.L _ _ (Remove e y)) x v ->
         Help.brkt (show e ++ " - " ++ y ++ " | " ++ x ++ " = " ++ show v)
     Insert e x v -> Help.brkt (show e ++ " | " ++ x ++ " = " ++ show v)
     Modify e fs -> Help.brkt (show e ++" | "++ intercalate ", " (map field fs))
         where field (x,e) = x ++ " <- " ++ show e
     Record r -> Help.brkt (intercalate ", " (map fields r))
         where fields (f,args,e) = f ++ concatMap (' ':) args ++ " = " ++ show e
     Markdown _ -> "[markdown| ... |]"


instance Show (Def t v) where
  show e =
   case e of
     TypeAnnotation n t -> n ++ " : " ++ show t
     Def name@(c:_) args e ->
         value ++ " = " ++ show e
       where
         value = if not (Help.isOp c) then name ++ concatMap (' ':) args else
                 case args of
                   [] -> parens name
                   [a,b] -> a ++ " " ++ name ++ " " ++ b

getLambdas (Location.L _ _ (Lambda x e)) = (show x:xs,e')
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
    Data name (x:xs) -> name /= "::"
    _ -> False
