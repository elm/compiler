{-# OPTIONS_GHC -Wall #-}

module AST.Expression.Valid where

import AST.PrettyPrint
import Text.PrettyPrint as P
import qualified AST.Expression.General as General
import AST.Type (RawType)
import qualified AST.Variable as Var
import qualified AST.Annotation as Annotation
import qualified AST.Pattern as Pattern


{-| "Normal" expressions. When the compiler checks that type annotations and
ports are all paired with definitions in the appropriate order, it collapses
them into a Def that is easier to work with in later phases of compilation.
-}
type Expr = General.Expr Annotation.Region Def Var.Raw
type Expr' = General.Expr' Annotation.Region Def Var.Raw

data Def = Definition Pattern.RawPattern Expr (Maybe RawType)
    deriving (Show)

instance Pretty Def where
  pretty (Definition pattern expr maybeTipe) =
      P.vcat [ annotation, definition ]
      where
        definition = pretty pattern <+> P.equals <+> pretty expr
        annotation = case maybeTipe of
                       Nothing -> P.empty
                       Just tipe -> pretty pattern <+> P.colon <+> pretty tipe
