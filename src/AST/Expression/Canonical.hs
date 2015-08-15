{-# OPTIONS_GHC -Wall #-}
module AST.Expression.Canonical where

import Text.PrettyPrint as P

import qualified AST.Expression.General as General
import qualified AST.Pattern as Pattern
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Region as R


{-| Canonicalized expressions. All variables are fully resolved to the module
they came from.
-}
type Expr =
  General.Expr R.Region Def Var.Canonical Type.Canonical


type Expr' =
  General.Expr' R.Region Def Var.Canonical Type.Canonical


data Def
    = Definition Pattern.CanonicalPattern Expr (Maybe (A.Located Type.Canonical))
    deriving (Show)


instance P.Pretty Def where
  pretty dealiaser _ (Definition pattern expr maybeTipe) =
      P.vcat [ annotation, definition ]
    where
      definition =
          P.pretty dealiaser True pattern
          <+> P.equals
          <+> P.pretty dealiaser False expr

      annotation =
          case maybeTipe of
            Nothing ->
                P.empty

            Just tipe ->
                P.pretty dealiaser True pattern
                <+> P.colon
                <+> P.pretty dealiaser False tipe
