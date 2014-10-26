module Type.Constrain.Literal where

import AST.Annotation
import AST.Literal
import Type.Type
import Type.Environment as Env

constrain :: Environment -> Region -> Literal -> Type -> IO TypeConstraint
constrain env region literal tipe =
    do tipe' <- litType
       return . A region $ CEqual tipe tipe'
    where
      prim name = return (Env.get env Env.types name)

      litType =
          case literal of
            IntNum _   -> varN `fmap` variable (Is Number)
            FloatNum _ -> prim "Float"
            Chr _      -> prim "Char"
            Str _      -> prim "String"
            Boolean _  -> prim "Bool"
