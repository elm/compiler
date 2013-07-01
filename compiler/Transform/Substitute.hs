module Transform.Substitute (subst) where

import SourceSyntax.Expression
import SourceSyntax.Location
import Data.Generics.Uniplate.Data
import Data.Data

subst :: (Data t, Data v) => String -> Expr t v -> Expr t v -> Expr t v
subst old new expr =
  transformBi substOne expr
    where
      substOne x@(Lambda name e) | name == old = new
      substOne x@(Var name)      | name == old = new
      substOne x = x
