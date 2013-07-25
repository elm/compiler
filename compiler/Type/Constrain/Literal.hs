module Type.Constrain.Literal where

import Data.Map ((!))
import qualified Data.Map as Map

import SourceSyntax.Literal
import Type.Type
import Type.Fragment
import Type.Environment as Env

constrain :: Environment -> Literal -> Type -> IO TypeConstraint
constrain env literal tipe =
    let prim name = Env.get env Env.types name in
    case literal of
      IntNum _   -> fmap (\n -> tipe === VarN n) number
      FloatNum _ -> return $ tipe === prim "Float"
      Chr _      -> return $ tipe === prim "Char"
      Str _      -> return $ tipe === TermN (App1 (prim "_List") (prim "Char"))
      Boolean _  -> return $ tipe === prim "Bool"
