
module SourceSyntax.Type where

import qualified Data.Map as Map
import SourceSyntax.PrettyPrint
import Text.PrettyPrint as P

data Type = Lambda Type Type
          | Var String
          | Data String [Type]
          | EmptyRecord
          | Record (Map.Map String [Type]) Type
            deriving (Eq, Show)

fieldMap :: [(String,Type)] -> Map.Map String [Type]
fieldMap fields =
    foldl (\r (x,t) -> Map.insertWith (++) x [t] r) Map.empty fields

recordOf :: [(String,Type)] -> Type
recordOf fields = Record (fieldMap fields) EmptyRecord

listOf :: Type -> Type
listOf t = Data "List" [t]

tupleOf :: [Type] -> Type
tupleOf ts = Data ("_Tuple" ++ show (length ts)) ts


instance Pretty Type where
  pretty tipe =
    case tipe of
      Lambda t1 t2 -> P.sep [ t, P.sep (map (P.text "->" <+>) ts) ]
        where t:ts = collectLambdas tipe
      Var x -> P.text x
      Data name tipes -> P.hang (P.text name) 2 (P.sep $ map prettyParens tipes)
      EmptyRecord -> P.braces P.empty
      Record fields ext -> error "not done yet"

collectLambdas tipe =
  case tipe of
    Lambda arg@(Lambda _ _) body -> P.parens (pretty arg) : collectLambdas body
    Lambda arg body -> pretty arg : collectLambdas body
    _ -> [pretty tipe]

prettyParens tipe = parensIf needed (pretty tipe)
  where
    needed =
      case tipe of
        Lambda _ _ -> True
        Data _ _ -> True
        _ -> False