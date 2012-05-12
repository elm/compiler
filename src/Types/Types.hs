
module Types where

import Data.List (intercalate)
import qualified Data.Set as Set

type X = Int

data Type = IntT
          | StringT
          | CharT
          | BoolT
          | LambdaT Type Type
          | VarT X
          | AppT String [Type]
          | ADT String [Type]
            deriving (Eq, Ord)

data Scheme = Forall (Set.Set X) Type deriving (Eq, Ord, Show)

data Constructor = Constructor String [Type] deriving (Eq, Show)

parens = ("("++) . (++")")

instance Show Type where
  show t =
      case t of
        { IntT -> "Int"
        ; StringT -> "String"
        ; CharT -> "Char"
        ; BoolT -> "Bool"
        ; LambdaT t1 t2 -> parens $ show t1 ++ " -> " ++ show t2
        ; VarT x -> show x
        ; AppT name args -> name ++ " " ++ unwords (map show args)
        ; ADT "List" [tipe] -> "[" ++ show tipe ++ "]"
        ; ADT name [] -> name
        ; ADT name cs -> parens $ name ++ " " ++ unwords (map show cs)
        }
