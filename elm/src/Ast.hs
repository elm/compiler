
module Ast where

import Data.Char (isDigit)
import Data.List (intercalate)
import Types
import Guid
import qualified Text.Pandoc as Pandoc

data Module = Module [String] Exports Imports [Statement]

type Exports = [String]

type Imports = [(String, ImportMethod)]
data ImportMethod = As String | Hiding [String] | Importing [String]


data Pattern = PData String [Pattern] | PVar String | PAnything
               deriving (Eq)

data Expr = IntNum Int
          | FloatNum Float
          | Chr Char
          | Str String
          | Boolean Bool
          | Range Expr Expr
          | Access Expr String
          | Binop String Expr Expr
          | Lambda String Expr
          | App Expr Expr
          | If Expr Expr Expr
          | Lift Expr [Expr]
          | Fold Expr Expr Expr
          | Async Expr
          | Input String
          | Let [Definition] Expr
          | Var String
          | Case Expr [(Pattern,Expr)]
          | Data String [Expr]
          | Markdown Pandoc.Pandoc
            deriving (Eq)

data Definition = Definition String [String] Expr
                  deriving (Eq)

data Statement = Def String [String] Expr
               | Datatype String [X] [(String,[Type])]
               | ImportEvent String Expr String Type
               | ExportEvent String String Type
                 deriving (Eq,Show)

cons h t = Data "Cons" [h,t]
nil      = Data "Nil" []
list     = foldr cons nil
tuple es = Data ("Tuple" ++ show (length es)) es

delist (Data "Cons" [h,t]) = h : delist t
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
  show (IntNum n) = show n
  show (FloatNum n) = show n
  show (Chr c) = show c
  show (Str s) = show s
  show (Boolean b) = show b
  show (Range e1 e2) = "[" ++ show e1 ++ ".." ++ show e2 ++ "]"
  show (Access e x) = show' e ++ "." ++ x
  show (Binop op e1 e2) = show' e1 ++ " " ++ op ++ " " ++ show' e2
  show (Lambda x e) = let (xs,e') = getLambdas (Lambda x e) in
                      concat [ "\\", intercalate " " xs, " -> ", show e' ]
  show (App e1 e2) = show' e1 ++ " " ++ show' e2
  show (If e1 e2 e3) = concat [ "if ", show e1, " then ", show e2, " else ", show e3 ]
  show (Let defs e) = "let { " ++ intercalate " ; " (map show defs) ++ " } in " ++ show e
  show (Var x) = x
  show (Case e pats) = "case " ++ show e ++ " of { " ++ intercalate " ; " (map (\(p,e) -> show p ++ " -> " ++ show e) pats) ++ " }"
  show (Data name es)
      | name == "Cons" = ("["++) . (++"]") . intercalate "," . map show $ delist (Data "Cons" es)
      | name == "Nil" = "[]"
      | otherwise = name ++ " " ++ intercalate " " (map show' es)
  show (Lift f es) = concat [ "lift", show $ length es, " ", show' f, " ", intercalate " " (map show' es) ]
  show (Fold e1 e2 e3) = concat [ "foldp ", show' e1, " ", show' e2, " ", show' e3 ]
  show (Async e) = "async " ++ show' e
  show (Input i) = i


instance Show Definition where
  show (Definition v [] e) = v ++ " = " ++ show e
  show (Definition f args e) = f ++ " " ++ intercalate " " args ++ " = " ++ show e

getLambdas (Lambda x e) = (x:xs,e')
    where (xs,e') = getLambdas e
getLambdas e = ([],e)

show' e = if needsParens e then "(" ++ show e ++ ")" else show e

needsParens (Binop _ _ _) = True
needsParens (Lambda _ _) = True
needsParens (App _ _) = True
needsParens (If _ _ _) = True
needsParens (Let _ _) = True
needsParens (Case _ _) = True
needsParens (Data name (x:xs)) = name /= "Cons"
needsParens (Lift _ _) = True
needsParens (Fold _ _ _) = True
needsParens (Async _) = True
needsParens _ = False
