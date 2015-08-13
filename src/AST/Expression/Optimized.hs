{-# OPTIONS_GHC -Wall #-}
module AST.Expression.Optimized
    ( Expr, Expr'
    , Def(..)
    , Facts(..), dummyFacts
    ) where

import qualified AST.Expression.General as General
import qualified AST.Pattern as Pattern
import qualified AST.Type as Type
import qualified AST.Variable as Var


type Expr =
  General.Expr () Def Var.Canonical Type.Canonical


type Expr' =
  General.Expr' () Def Var.Canonical Type.Canonical


data Def
    = Definition Facts Pattern.Optimized Expr
    deriving (Show)


data Facts = Facts
    { tailRecursionDetails :: Maybe String
    }
    deriving (Show)


dummyFacts :: Facts
dummyFacts =
    Facts Nothing