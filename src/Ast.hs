module Ast where

data Pattern = PData String [Pattern] | PVar String | PAnything
               deriving (Show, Eq)

data Expr = Number Int
          | Chr Char
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
          | Let [(String,Expr)] Expr
          | Var String
          | Case Expr [(Pattern,Expr)]
          | Data String [Expr]
            deriving (Show, Eq)

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
