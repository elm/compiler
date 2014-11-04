{-# OPTIONS_GHC -Wall #-}

module AST.Expression.Canonical where

import AST.PrettyPrint
import Text.PrettyPrint as P
import qualified AST.Expression.General as General
import AST.Type (CanonicalType)
import qualified AST.Annotation as Annotation
import qualified AST.Pattern as Pattern
import qualified AST.Variable as Var


{-| Canonicalized expressions. All variables are fully resolved to the module
they came from.
-}
type Expr = General.Expr Annotation.Region Def Var.Canonical
type Expr' = General.Expr' Annotation.Region Def Var.Canonical

data Def = Definition Pattern.CanonicalPattern Expr (Maybe CanonicalType)
    deriving (Show)

instance Pretty Def where
  pretty (Definition pattern expr maybeTipe) =
      P.vcat [ annotation, definition ]
      where
        definition = pretty pattern <+> P.equals <+> pretty expr
        annotation = case maybeTipe of
                       Nothing -> P.empty
                       Just tipe -> pretty pattern <+> P.colon <+> pretty tipe
