module Type.Constrain.Declaration where

import Type.Type
import SourceSyntax.Declaration
import qualified Type.Constrain.Expression as Expr
import qualified Type.Environment as Env

constrain :: Env.Environment -> Declaration t v -> Type -> IO TypeConstraint
constrain env decl tipe =
  case decl of
    ImportEvent _ expr var sourceType -> do
      etipe <- (Expr.instantiateType sourceType)
      constraint <- Expr.constrain env expr etipe
      return (tipe === etipe /\ constraint)
      -- also do something with var. Not sure what yet.

    ExportEvent _ var sourceType -> do
      etipe <- (Expr.instantiateType sourceType)
      return (tipe === etipe)
      -- also do something with var. Not sure what yet.
