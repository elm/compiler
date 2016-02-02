{-# OPTIONS_GHC -Wall #-}
module AST.Expression.Canonical where

import qualified AST.Expression.General as General
import qualified AST.Pattern as Pattern
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


{-| Canonicalized expressions. All variables are fully resolved to the module
they came from.
-}
type Expr =
  General.Expr R.Region Def Var.Canonical Type.Canonical


type Expr' =
  General.Expr' R.Region Def Var.Canonical Type.Canonical


data Def
    = Definition Facts Pattern.CanonicalPattern Expr (Maybe (A.Located Type.Canonical))


data Facts = Facts
    { dependencies :: [Var.TopLevel]
    }


dummyFacts :: Facts
dummyFacts =
  Facts (error "This should be set by Canonicalize.Sort")

