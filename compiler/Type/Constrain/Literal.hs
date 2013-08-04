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
      prim name = Env.get env Env.types name

      litType =
          case literal of
            IntNum _   -> VarN `fmap` var (Is Number)
            FloatNum _ -> return (prim "Float")
            Chr _      -> return (prim "Char")
            Str _      -> return (TermN (App1 (prim "_List") (prim "Char")))
            Boolean _  -> return (prim "Bool")
