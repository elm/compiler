{-# OPTIONS_GHC -Wall #-}
module AST.Expression.Optimized
    ( Def(..), Facts(..), dummyFacts
    , Expr(..), Jump(..), Branch(..)
    ) where

import qualified AST.Expression.General as General
import qualified AST.Literal as Literal
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Optimize.Patterns.DecisionTree as DT
import qualified Reporting.Region as R



-- DEFINITIONS

data Def
    = Def Facts String Expr
    | TailDef Facts String [String] Expr


data Facts = Facts
    { home :: Maybe ModuleName.Canonical
    , dependencies :: [Var.TopLevel]
    }


dummyFacts :: Facts
dummyFacts =
  Facts Nothing []


-- EXPRESSIONS

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
    | Case String (DT.DecisionTree Jump) [(Int, Branch)]
    | Data String [Expr]
    | DataAccess Expr Int
    | Access Expr String
    | Update Expr [(String, Expr)]
    | Record [(String, Expr)]
    | Port (General.PortImpl Expr Type.Canonical)
    | GLShader String String Literal.GLShaderTipe
    | Crash R.Region (Maybe String)


data Jump
    = Inline Branch
    | Jump Int


data Branch = Branch
    { _substitutions :: [(String, DT.Path)]
    , _branch :: Expr
    }