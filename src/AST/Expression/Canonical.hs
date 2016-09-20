{-# OPTIONS_GHC -Wall #-}
module AST.Expression.Canonical
  ( Expr, Expr'
  , Def(..)
  , SortedDefs(..), toSortedDefs
  )
  where

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
    = Def R.Region Pattern.Canonical Expr (Maybe (A.Located Type.Canonical))



-- SORTED DEFS


data SortedDefs
  = NoMain [Def]
  | YesMain [Def] Def [Def]


toSortedDefs :: Expr -> SortedDefs
toSortedDefs (A.A _ expr) =
  case expr of
    General.Let defs body ->
      foldr defCons (toSortedDefs body) defs

    _ ->
      NoMain []


defCons :: Def -> SortedDefs -> SortedDefs
defCons def@(Def _ (A.A _ pattern) _ _) sortedDefs =
  case (pattern, sortedDefs) of
    (Pattern.Var "main", NoMain defs) ->
      YesMain [] def defs

    (_, NoMain defs) ->
      NoMain (def : defs)

    (_, YesMain defs main rest) ->
      YesMain (def : defs) main rest
