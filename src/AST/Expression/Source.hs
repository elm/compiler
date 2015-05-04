{-# OPTIONS_GHC -Wall #-}
module AST.Expression.Source where

import qualified AST.Expression.General as General
import qualified AST.Pattern as Pattern
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


{-| Expressions created by the parser. These use a split representation of type
annotations and definitions, which is how they appear in source code and how
they are parsed.
-}
type Expr =
  General.Expr R.Region Def Var.Raw Type.Raw


type Expr' =
  General.Expr' R.Region Def Var.Raw Type.Raw


type Def =
  A.Located Def'


data Def'
    = Definition Pattern.RawPattern Expr
    | TypeAnnotation String Type.Raw
    deriving (Show)
