
module Ast where

import Data.Char (isDigit)
import Data.List (intercalate)
import Types
import Guid

data Module = Module [String] Exports Imports Defs [JSFFI]

type Exports = [String]

type Imports = [(String, ImportMethod)]
data ImportMethod = As String | Hiding [String] | Importing [String]

type Defs = [(String,Expr)]

data JSFFI = ImportValue String String Type
           | ExportValue String String Type
           | ImportEvent String String Expr Type
           | ExportEvent String String Type


data Pattern = PData String [Pattern] | PVar String | PAnything
               deriving (Eq)

data Expr = Number Int
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