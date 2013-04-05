
module Types.Types where

import Context
import Data.Char (isDigit)
import Data.List (intercalate,isPrefixOf)
import qualified Data.Set as Set
import qualified Data.Map as Map

type X = Int

data Type = LambdaT Type Type
          | VarT X
          | ADT String [Type]
          | EmptyRecord
          | RecordT (Map.Map String [Type]) Type
          | Super (Set.Set Type)
            deriving (Eq, Ord)

data Scheme = Forall [X] [Context Constraint] Type deriving (Eq, Ord, Show)

data Constraint = Type :=: Type
                | Type :<: Type
                | X :<<: Scheme
                  deriving (Eq, Ord, Show)

recordT :: [(String,Type)] -> Map.Map String [Type]
recordT fields =
    foldl (\r (x,t) -> Map.insertWith (++) x [t] r) Map.empty fields

recordOf :: [(String,Type)] -> Type
recordOf fields = RecordT (recordT fields) EmptyRecord

tipe t = ADT t []

int = tipe "Int"
float = tipe "Float"
number = Super (Set.fromList [ int, float ])

char = tipe "Char"
bool = tipe "Bool"

string = tipe "String"
text  = tipe "Text"

element   = tipe "Element"

listOf t   = ADT "List" [t]
signalOf t = ADT "Signal" [t]
tupleOf ts = ADT ("Tuple" ++ show (length ts)) ts
maybeOf t  = ADT "Maybe" [t]
eitherOf a b = ADT "Either" [a,b]
pairOf t = tupleOf [t,t]
point = pairOf int
appendable t = Super (Set.fromList [ string, text, listOf t ])
--comparable = Super (Set.fromList [ int, float, char, string, time, date ])

infixr ==>
t1 ==> t2 = LambdaT t1 t2

infix 8 -:
name -: tipe = (,) name $ Forall [] [] tipe

parens = ("("++) . (++")")

instance Show Type where
  show t =
   let show' t = case t of { LambdaT _ _ -> parens (show t) ; _ -> show t }
   in case t of
      LambdaT t1@(LambdaT _ _) t2 -> show' t1 ++ " -> " ++ show t2
      LambdaT t1 t2 -> show t1 ++ " -> " ++ show t2
      VarT x -> 't' : show x
      --ADT "List" [ADT "Char" []] -> "String"
      ADT "List" [tipe] -> "[" ++ show tipe ++ "]"
      ADT name cs ->
          if isTupleString name
              then parens . intercalate "," $ map show cs
              else case cs of
                     [] -> name
                     _  -> name ++ " " ++ unwords (map show cs)
      Super ts -> "{" ++ (intercalate "," . map show $ Set.toList ts) ++ "}"
      EmptyRecord -> "{}"
      RecordT fs t ->
        start ++ intercalate ", " (concatMap fields $ Map.toList fs) ++ " }"
           where field n s = n ++ " : " ++ show s
                 fields (n,ss) = map (field n) ss
                 start = case t of
                           EmptyRecord -> "{ "
                           _ -> "{ " ++ show t ++ " | "


isTupleString str = "Tuple" `isPrefixOf` str && all isDigit (drop 5 str)