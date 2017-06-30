{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Constrain.Literal where

import qualified AST.Literal as L
import qualified AST.Variable as Var
import qualified Reporting.Error.Type as Error
import qualified Reporting.Region as R
import qualified Type.Type as T



-- CONSTRAIN LITERALS


constrain :: R.Region -> L.Literal -> T.Type -> IO T.Constraint
constrain region literal tipe =
  do  definiteType <- litType
      return (T.CEqual (Error.Literal name) region definiteType tipe)
  where
    prim var =
        return (T.AppN var [])

    (name, litType) =
        case literal of
          L.IntNum _ ->
              ( "number"
              , T.VarN <$> T.mkVar (Just T.Number)
              )

          L.FloatNum _ ->
              ( "float"
              , prim Var.float
              )

          L.Chr _ ->
              ( "character"
              , prim Var.char
              )

          L.Str _ ->
              ( "string"
              , prim Var.string
              )

          L.Boolean _ ->
              ( "boolean"
              , prim Var.bool
              )
