module Elm.Compiler.Type.Extract (toFullType, toAliasedType) where

import Control.Arrow (second)
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Elm.Compiler.Type as T


toFullType :: Type.Canonical -> T.Type
toFullType astType =
  toType False astType


toAliasedType :: Type.Canonical -> T.Type
toAliasedType astType =
  toType True astType


toType :: Bool -> Type.Canonical -> T.Type
toType useAliases astType =
  let
    go = toType useAliases
  in
    case astType of
      Type.Lambda t1 t2 ->
          T.Lambda (go t1) (go t2)

      Type.Var x ->
          T.Var x

      Type.Type var ->
          T.Type (Var.toString var)

      Type.App t ts ->
          T.App (go t) (map go ts)

      Type.Record fields ext ->
          T.Record (map (second go) fields) (fmap go ext)

      Type.Aliased name args t ->
        if useAliases then
          T.App (T.Type (Var.toString name)) (map (go . snd) args)
        else
          go (Type.dealias args t)