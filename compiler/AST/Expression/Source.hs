{-# OPTIONS_GHC -Wall #-}

module AST.Expression.Source where

import SourceSyntax.PrettyPrint
import Text.PrettyPrint as P
import qualified AST.Expression.General as General
import AST.Type (Type)
import qualified AST.Variable as Var
import qualified SourceSyntax.Annotation as Annotation
import qualified SourceSyntax.Pattern as Pattern


{-| Expressions created by the parser. These use a split representation of type
annotations and definitions, which is how they appear in source code and how
they are parsed.
-}
type Expr = General.Expr Annotation.Region Def Var.Raw
type Expr' = General.Expr' Annotation.Region Def Var.Raw

data Def
    = Definition (Pattern.Pattern Var.Raw) Expr
    | TypeAnnotation String (Type Var.Raw)
    deriving (Show)

instance Pretty Def where
  pretty def =
   case def of
     TypeAnnotation name tipe ->
         variable name <+> P.colon <+> pretty tipe
     Definition pattern expr ->
         pretty pattern <+> P.equals <+> pretty expr
