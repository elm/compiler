
module Types where

import Data.List (intercalate)
import Data.IORef
import System.IO.Unsafe

data Type = IntT
          | StringT
          | CharT
          | BoolT
          | LambdaT Type Type
          | VarT String
          | ForallT String Type
          | AppT String [Type]
          | ADT String [Type]
            deriving (Eq)

data Constructor = Constructor String [Type] deriving (Eq, Show)

instance Show Type where
  show t =
      case t of
        { IntT -> "Int"
        ; StringT -> "String"
        ; CharT -> "Char"
        ; BoolT -> "Bool"
        ; LambdaT t1 t2 -> show t1 ++ " -> " ++ show t2
        ; VarT x -> x
        ; ForallT x t' -> "forall " ++ x ++ ". " ++ show t'
        ; AppT name args -> name ++ " " ++ intercalate " " (map show args)
        ; ADT name constrs ->
            name ++ " = " ++ intercalate " | " (map show constrs)
        }
