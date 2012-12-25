
module Ast where

import Context
import Data.Char (isDigit)
import Data.List (intercalate)
import Types.Types
import qualified Text.Pandoc as Pandoc

data Module = Module [String] Exports Imports [Statement]

type Exports = [String]

type Imports = [(String, ImportMethod)]
data ImportMethod = As String | Hiding [String] | Importing [String]
                    deriving (Eq,Ord)


data Pattern = PData String [Pattern] | PVar String | PAnything
               deriving (Eq)

type CExpr = Context Expr
data Expr = IntNum Int
          | FloatNum Float
          | Chr Char
          | Str String
          | Boolean Bool
          | Range CExpr CExpr
          | Access CExpr String
          | Record [(String,[String],CExpr)]
          | Binop String CExpr CExpr
          | Lambda String CExpr
          | App CExpr CExpr
          | If CExpr CExpr CExpr
          | MultiIf [(CExpr,CExpr)]
          | Let [Def] CExpr
          | Var String
          | Case CExpr [(Pattern,CExpr)]
          | Data String [CExpr]
          | Markdown Pandoc.Pandoc
            deriving (Eq)

data Def = FnDef String [String] CExpr
         | OpDef String String String CExpr
           deriving (Eq)

data Statement = Definition Def
               | Datatype String [X] [(String,[Type])]
               | ImportEvent String CExpr String Type
               | ExportEvent String String Type
                 deriving (Eq,Show)

cons h t = epos h t (Data "Cons" [h,t])
nil      = C (Just "[]") NoSpan (Data "Nil" [])
list     = foldr cons nil
tuple es = Data ("Tuple" ++ show (length es)) es

delist (C _ _ (Data "Cons" [h,t])) = h : delist t
delist _ = []


pcons h t = PData "Cons" [h,t]
pnil      = PData "Nil" []
plist     = foldr pcons pnil
ptuple es = PData ("Tuple" ++ show (length es)) es

instance Show Pattern where
    show (PVar x)  = x
    show PAnything = "_"
    show (PData "Cons" [hd@(PData "Cons" _),tl]) =
        parens (show hd) ++ " : " ++ show tl
            where parens s = "(" ++ s ++ ")"
    show (PData "Cons" [hd,tl]) = show hd ++ " : " ++ show tl
    show (PData "Nil" []) = "[]"
    show (PData name ps) =
        if take 5 name == "Tuple" && all isDigit (drop 5 name) then
            parens . intercalate ", " $ map show ps
        else (if null ps then id else parens) $ unwords (name : map show ps)
            where parens s = "(" ++ s ++ ")"

instance Show Expr where
  show e =
   case e of
     IntNum n -> show n
     FloatNum n -> show n
     Chr c -> show c
     Str s -> show s
     Boolean b -> show b
     Range e1 e2 -> "[" ++ show e1 ++ ".." ++ show e2 ++ "]"
     Access e x -> show' e ++ "." ++ x
     Record r -> "{" ++ intercalate ", " (map fields r) ++ "}"
         where fields (f,args,e) = f ++ concatMap (' ':) args ++ " = " ++ show e
     Binop op e1 e2 -> show' e1 ++ " " ++ op ++ " " ++ show' e2
     Lambda x e -> let (xs,e') = getLambdas (noContext $ Lambda x e) in
                      concat [ "\\", intercalate " " xs, " -> ", show e' ]
     App e1 e2 -> show' e1 ++ " " ++ show' e2
     If e1 e2 e3 -> concat [ "if ", show e1, " then ", show e2, " else ", show e3 ]
     MultiIf (p:ps) -> concat [ "if | ", iff p, sep (map iff ps) ]
         where iff (b,e) = show b ++ " -> " ++ show e
               sep = concatMap ("\n   | " ++)
     Let defs e -> "let { "++intercalate " ; " (map show defs)++" } in "++show e
     Var x -> x
     Case e pats -> "case "++ show e ++" of { "++ intercalate " ; " pats' ++" }"
         where pats' = map (\(p,e) -> show p ++ " -> " ++ show e) pats
     Data name es
          | name == "Cons" -> ("["++) . (++"]") . intercalate "," . map show $
                              delist (noContext $ Data "Cons" es)
          | name == "Nil"  -> "[]"
          | otherwise      -> name ++ " " ++ intercalate " " (map show' es)
     Markdown _ -> "[markdown| ... |]"


instance Show Def where
  show e =
   case e of
     FnDef v [] e     -> v ++ " = " ++ show e
     FnDef f args e   -> f ++ " " ++ intercalate " " args ++ " = " ++ show e
     OpDef op a1 a2 e -> intercalate " " [a1,op,a2] ++ " = " ++ show e

getLambdas (C _ _ (Lambda x e)) = (x:xs,e')
    where (xs,e') = getLambdas e
getLambdas e = ([],e)

show' (C _ _ e) = if needsParens e then "(" ++ show e ++ ")" else show e

needsParens (Binop _ _ _) = True
needsParens (Lambda _ _) = True
needsParens (App _ _) = True
needsParens (If _ _ _) = True
needsParens (Let _ _) = True
needsParens (Case _ _) = True
needsParens (Data name (x:xs)) = name /= "Cons"
needsParens _ = False
