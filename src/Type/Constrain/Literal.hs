module Type.Constrain.Literal where

import qualified AST.Literal as L
import qualified Reporting.Error.Type as Error
import qualified Reporting.Region as R
import qualified Type.Type as T
import qualified Type.Environment as Env


constrain :: Env.Env -> R.Region -> L.Literal -> T.Type -> IO T.TypeConstraint
constrain env region literal tipe =
  do  definiteType <- litType
      return (T.CEqual (Error.Literal name) region definiteType tipe)
  where
    prim name =
        return (Env.getType env name)

    (name, litType) =
        case literal of
          L.IntNum _ ->
              ( "number"
              , T.VarN <$> T.mkVar (Just T.Number)
              )

          L.FloatNum _ ->
              ( "float"
              , prim "Float"
              )

          L.Chr _ ->
              ( "character"
              , prim "Char"
              )

          L.Str _ ->
              ( "string"
              , prim "String"
              )

          L.Boolean _ ->
              ( "boolean"
              , prim "Bool"
              )
