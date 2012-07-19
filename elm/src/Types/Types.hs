
module Types where

import Data.Char (isDigit)
import Data.List (intercalate,isPrefixOf)
import qualified Data.Set as Set

type X = Int

data Type = LambdaT Type Type
          | VarT X
          | ADT String [Type]
            deriving (Eq, Ord)

data Scheme = Forall [X] [Constraint] Type deriving (Eq, Ord, Show)

data SuperType = SuperType String (Set.Set Type) deriving (Eq, Ord)

data Constraint = Type :=: Type
                | Type :<: SuperType
                | Type :<<: Scheme
                  deriving (Eq, Ord, Show)

tipe t = ADT t []


int = tipe "Int"
float = tipe "Float"
number = SuperType "Number" (Set.fromList [ int, float ])

char = tipe "Char"
bool = tipe "Bool"

string = tipe "String"
text  = tipe "Text"

time = SuperType "Time" (Set.fromList [ int, float ])

element   = tipe "Element"
direction = tipe "Direction"
form  = tipe "Form"
line  = tipe "Line"
shape = tipe "Shape"
color = tipe "Color"
point = tupleOf [int,int]

listOf t   = ADT "List" [t]
signalOf t = ADT "Signal" [t]
tupleOf ts = ADT ("Tuple" ++ show (length ts)) ts
maybeOf t  = ADT "Maybe" [t]

jsBool     = tipe "JSBool"
jsNumber   = tipe "JSNumber"
jsString   = tipe "JSString"
jsElement  = tipe "JSElement"
jsArray t  = ADT "JSArray" [t]
jsTuple ts = ADT ("JSTuple" ++ show (length ts)) ts

infixr ==>
t1 ==> t2 = LambdaT t1 t2

infix 8 -:
name -: tipe = (,) name $ Forall [] [] tipe

hasType t = map (-: t)

parens = ("("++) . (++")")

instance Show Type where
  show t =
      case t of
        { LambdaT t1@(LambdaT _ _) t2 -> parens (show t1) ++ " -> " ++ show t2
        ; LambdaT t1 t2 -> show t1 ++ " -> " ++ show t2
        ; VarT x -> show x
        ; ADT "List" [tipe] -> "[" ++ show tipe ++ "]"
        ; ADT name cs ->
            if isTupleString name
                then parens . intercalate "," $ map show cs
                else case cs of [] -> name
                                _ -> parens $ name ++ " " ++ unwords (map show cs)
        }

instance Show SuperType where
  show (SuperType n ts) = "" ++ n ++ " (a type in {" ++ subs ++ "})"
      where subs = intercalate "," . map show $ Set.toList ts

isTupleString str = "Tuple" `isPrefixOf` str && all isDigit (drop 5 str)