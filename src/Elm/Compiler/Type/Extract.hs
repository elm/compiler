module Elm.Compiler.Type.Extract where

import Control.Arrow (second)
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Elm.Compiler.Type as T


-- INTERNAL TYPE to PUBLIC TYPE

fromInternalType :: Var.ToString a => Type.Type a -> T.Type
fromInternalType astType =
    case astType of
      Type.Lambda t1 t2 ->
          T.Lambda (fromInternalType t1) (fromInternalType t2)

      Type.Var x ->
          T.Var x

      Type.Type var ->
          T.Type (Var.toString var)

      Type.App t ts ->
          T.App (fromInternalType t) (map fromInternalType ts)

      Type.Record fields ext ->
          T.Record (map (second fromInternalType) fields) (fmap fromInternalType ext)

      Type.Aliased _ t ->
          fromInternalType t