{-# OPTIONS_GHC -Wall #-}
module AST.Expression.Valid where

import qualified AST.Expression.General as General
import qualified AST.Pattern as Pattern
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Reporting.Region as R


{-| "Normal" expressions. When the compiler checks that type annotations and
ports are all paired with definitions in the appropriate order, it collapses
them into a Def that is easier to work with in later phases of compilation.
-}
type Expr =
  General.Expr R.Region Def Var.Raw Type.Raw


type Expr' =
  General.Expr' R.Region Def Var.Raw Type.Raw


data Def
    = Definition Pattern.RawPattern Expr (Maybe Type.Raw)
    deriving (Show)
