{-# OPTIONS_GHC -W #-}
module Type.Constrain.Literal where

import SourceSyntax.Annotation
import SourceSyntax.Literal
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
            IntNum _   -> VarN `fmap` var (Is Number)
            FloatNum _ -> prim "Float"
            Chr _      -> prim "Char"
            Str _      -> prim "String"
            Boolean _  -> prim "Bool"
