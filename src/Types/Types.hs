
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
          | ADT String [Type]
            deriving (Eq, Ord)

data Scheme = Forall (Set.Set X) Type deriving (Eq, Ord, Show)

element   = ADT "Element" []
direction = ADT "Direction" []

form  = ADT "Form" []
line  = ADT "Line" []
shape = ADT "Shape" []
color = ADT "Color" []
text  = ADT "List" [ADT "Text" []]
point = tupleOf [IntT,IntT]

listOf t   = ADT "List" [t]
signalOf t = ADT "Signal" [t]
tupleOf ts = ADT ("Tuple" ++ show (length ts)) ts
string     = listOf CharT

infixr ==>
t1 ==> t2 = LambdaT t1 t2

infix 8 -:
name -: tipe = (,) name tipe

hasType t = map (-: t)

parens = ("("++) . (++")")

instance Show Type where
  show t =
      case t of
        { IntT -> "Int"
        ; StringT -> "String"
        ; CharT -> "Char"
        ; BoolT -> "Bool"
        ; LambdaT t1@(LambdaT _ _) t2 -> parens (show t1) ++ " -> " ++ show t2
        ; LambdaT t1 t2 -> show t1 ++ " -> " ++ show t2
        ; VarT x -> show x
        ; ADT "List" [tipe] -> "[" ++ show tipe ++ "]"
        ; ADT name [] -> name
        ; ADT name cs -> parens $ name ++ " " ++ unwords (map show cs)
        }
