
module Types.Types where

import Data.Char (isDigit)
import Data.List (intercalate,isPrefixOf)
import qualified Data.Set as Set

type X = Int

data Type = LambdaT Type Type
          | VarT X
          | ADT String [Type]
          | Super (Set.Set Type)
            deriving (Eq, Ord)

data Scheme = Forall [X] [Context String Constraint] Type deriving (Eq, Ord)

data Constraint = Type :=: Type
                | Type :<: Type
                | X :<<: Scheme
                  deriving (Eq, Ord, Show)

data Context c v = Context c v deriving (Eq, Show)

ctx c = Context ("`" ++ show c ++ "'")
extendCtx ctx2 (Context ctx1 c) = Context (ctx1 ++ " in " ++ ctx2) c

instance (Ord a, Eq c) => Ord (Context c a) where
    compare (Context _ x) (Context _ y) = compare x y


tipe t = ADT t []


int = tipe "Int"
float = tipe "Float"
number = Super (Set.fromList [ int, float ])

char = tipe "Char"
bool = tipe "Bool"

string = listOf char -- tipe "String"
text  = tipe "Text"

time = float --tipe "Time"
date = tipe "Date"
month = tipe "Month"
day = tipe "Day"

element   = tipe "Element"
direction = tipe "Direction"
form  = tipe "Form"
line  = tipe "Line"
shape = tipe "Shape"
color = tipe "Color"
position = tipe "Position"
location = tipe "Location"

listOf t   = ADT "List" [t]
signalOf t = ADT "Signal" [t]
tupleOf ts = ADT ("Tuple" ++ show (length ts)) ts
maybeOf t  = ADT "Maybe" [t]
pairOf t = tupleOf [t,t]
point = pairOf int
appendable t = Super (Set.fromList [ string, text, listOf t ])
comparable = Super (Set.fromList [ int, float, char, string, time, date ])

jsBool     = tipe "JSBool"
jsNumber   = tipe "JSNumber"
jsString   = tipe "JSString"
jsElement  = tipe "JSElement"
jsArray t  = ADT "JSArray" [t]
jsTuple ts = ADT ("JSTuple" ++ show (length ts)) ts
jsonValue = tipe "JsonValue"
jsonObject = tipe "JsonObject"

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
        ; VarT x -> 't' : show x
        ; ADT "List" [ADT "Char" []] -> "String"
        ; ADT "List" [tipe] -> "[" ++ show tipe ++ "]"
        ; ADT name cs ->
            if isTupleString name
                then parens . intercalate "," $ map show cs
                else case cs of [] -> name
                                _ -> parens $ name ++ " " ++ unwords (map show cs)
        ; Super ts -> "{" ++ (intercalate "," . map show $ Set.toList ts) ++ "}"
        }

instance Show Scheme where
  show (Forall xs cs t) = "Forall " ++ show xs ++ cs' ++ "\n  " ++ show t
      where cs' = concatMap (concatMap ("\n  "++) . lines . show) cs

isTupleString str = "Tuple" `isPrefixOf` str && all isDigit (drop 5 str)