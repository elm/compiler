{-# OPTIONS_GHC -Wall #-}

module AST.Expression.Canonical where

import SourceSyntax.PrettyPrint
import Text.PrettyPrint as P
import qualified AST.Expression.General as General
import AST.Type (Type)
import qualified AST.Variable as Var
import qualified SourceSyntax.Annotation as Annotation
import qualified SourceSyntax.Pattern as Pattern


{-| Canonicalized expressions. All variables are fully resolved to the module
they came from.
-}
type Expr = General.Expr Annotation.Region Def Var.Canonical
type Expr' = General.Expr' Annotation.Region Def Var.Canonical

data Def = Definition (Pattern.Pattern Var.Canonical) Expr (Maybe (Type Var.Canonical))
    deriving (Show)

instance Pretty Def where
  pretty (Definition pattern expr maybeTipe) =
      P.vcat [ annotation, definition ]
      where
        definition = pretty pattern <+> P.equals <+> pretty expr
        annotation = case maybeTipe of
                       Nothing -> P.empty
                       Just tipe -> pretty pattern <+> P.colon <+> pretty tipe
