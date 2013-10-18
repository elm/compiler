module Type.Constrain.Literal where

import SourceSyntax.Literal
import SourceSyntax.Location
import Type.Type
import Type.Environment as Env

constrain :: Environment -> SrcSpan -> Literal -> Type -> IO TypeConstraint
constrain env span literal tipe =
    do tipe' <- litType
       return . L span $ CEqual tipe tipe'
    where
      prim name = return (Env.get env Env.types name)

      litType =
          case literal of
            IntNum _   -> VarN `fmap` var (Is Number)
            FloatNum _ -> prim "Float"
            Chr _      -> prim "Char"
            Str _      -> prim "String"
            Boolean _  -> prim "Bool"
