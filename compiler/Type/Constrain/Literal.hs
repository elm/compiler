module Type.Constrain.Literal where

import Data.Map ((!))
import qualified Data.Map as Map

import SourceSyntax.Literal
import Type.Type
import Type.Fragment
import Type.Environment as Env

constrain :: Environment -> Literal -> Type -> TypeConstraint
constrain env literal tipe =
    let prim name = Env.get env builtin name in
    case literal of
      IntNum _   -> tipe === prim "Int"
      FloatNum _ -> tipe === prim "Float"
      Chr _      -> tipe === prim "Char"
      Str _      -> tipe === TermN (App1 (prim "[]") (prim "Char"))
      Boolean _  -> tipe === prim "Bool"
