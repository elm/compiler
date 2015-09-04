{-# OPTIONS_GHC -Wall #-}
module AST.Expression.Optimized
    ( Expr(..)
    , Def(..)
    , Facts(..)
    ) where

import qualified Data.Map as Map

import qualified AST.Expression.General as General
import qualified AST.Literal as Literal
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Optimize.Cases as PM


data Expr
    = Literal Literal.Literal
    | Var Var.Canonical
    | Range Expr Expr
    | ExplicitList [Expr]
    | Binop Var.Canonical Expr Expr
    | Function [String] Expr
    | Call Expr [Expr]
    | TailCall String [String] [Expr]
    | If [(Expr, Expr)] Expr
    | Let [Def] Expr
    | Case Expr (PM.DecisionTree Int) (Map.Map Int Expr)
    | Data String [Expr]
    | DataAccess Expr Int
    | Access Expr String
    | Update Expr [(String, Expr)]
    | Record [(String, Expr)]
    | Port (General.PortImpl Expr Type.Canonical)
    | GLShader String String Literal.GLShaderTipe
    | Crash


data Def
    = Def Facts String Expr
    | TailDef Facts String [String] Expr


data Facts = Facts
    { home :: Maybe ModuleName.Canonical
    , dependencies :: [Var.TopLevel]
    }
