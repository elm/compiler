module Type.Constrain.Literal where

import qualified AST.Literal as L
import qualified Reporting.Error.Type as Error
import qualified Reporting.Region as R
import qualified Type.Type as T
import qualified Type.Environment as Env


constrain
    :: Env.Environment
    -> R.Region
    -> L.Literal
    -> T.Type
    -> IO T.TypeConstraint
constrain env region literal tipe =
  do  tipe' <- litType
      return (T.CEqual (Error.Literal name) region tipe tipe')
  where
    prim name =
        return (Env.get env Env.types name)

    litType =
        case literal of
          L.IntNum _   -> T.varN `fmap` T.variable (T.Is T.Number)
          L.FloatNum _ -> prim "Float"
          L.Chr _      -> prim "Char"
          L.Str _      -> prim "String"
          L.Boolean _  -> prim "Bool"

    name =
        case literal of
          L.IntNum _   -> "number"
          L.FloatNum _ -> "float"
          L.Chr _      -> "character"
          L.Str _      -> "string"
          L.Boolean _  -> "boolean"
