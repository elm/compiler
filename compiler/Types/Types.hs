{-# LANGUAGE DeriveDataTypeable #-}
module Types.Types where

import Located
import Data.Char (isDigit)
import Data.List (intercalate,isPrefixOf)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Data

type X = Int

data Type = LambdaT Type Type
          | VarT X
          | ADT String [Type]
          | EmptyRecord
          | RecordT (Map.Map String [Type]) Type
          | Super (Set.Set Type)
            deriving (Eq, Ord, Data, Typeable)

data Scheme = Forall [X] [Located Constraint] Type
              deriving (Eq, Ord, Show, Data, Typeable)

data Constraint = Type :=: Type
                | Type :<: Type
                | X :<<: Scheme
                  deriving (Eq, Ord, Show, Data, Typeable)

recordT :: [(String,Type)] -> Map.Map String [Type]
recordT fields =
    foldl (\r (x,t) -> Map.insertWith (++) x [t] r) Map.empty fields

recordOf :: [(String,Type)] -> Type
recordOf fields = RecordT (recordT fields) EmptyRecord

tipe t = ADT t []

int = tipe "Int"
float = tipe "Float"
time = tipe "Time"
date = tipe "Date"

char = tipe "Char"
bool = tipe "Bool"
text = tipe "Text"
order = tipe "Order"
string = tipe "String"

number = Super $ Set.fromList [ int, float, time ]
appendable t = Super $ Set.fromList [ string, text, listOf (VarT t) ]
comparable t = Super $ Set.fromList [ int, float, char, string, time, date ]

element   = tipe "Element"

listOf t   = ADT "List" [t]
signalOf t = ADT "Signal" [t]
tupleOf ts = ADT ("Tuple" ++ show (length ts)) ts
maybeOf t  = ADT "Maybe" [t]
eitherOf a b = ADT "Either" [a,b]
pairOf t = tupleOf [t,t]
point = pairOf int

infixr ==>
t1 ==> t2 = LambdaT t1 t2

infix 8 -:
name -: tipe = (,) name $ Forall [] [] tipe

parens = ("("++) . (++")")

instance Show Type where
  show t =
   let addParens (c:cs) =
           if notElem ' ' cs || c == '(' then c:cs else parens (c:cs)
   in case t of
      LambdaT t1@(LambdaT _ _) t2 -> parens (show t1) ++ " -> " ++ show t2
      LambdaT t1 t2 -> show t1 ++ " -> " ++ show t2
      VarT x -> 't' : show x
      ADT "List" [ADT "Char" []] -> "String"
      ADT "List" [tipe] -> "[" ++ show tipe ++ "]"
      ADT name cs ->
          if isTupleString name
              then parens . intercalate "," $ map show cs
              else name ++ concatMap ((' ':) . addParens . show) cs
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