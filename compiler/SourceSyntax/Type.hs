{-# LANGUAGE DeriveDataTypeable #-}
module SourceSyntax.Type where

import Data.Data
import qualified Data.Map as Map
import qualified SourceSyntax.Helpers as Help
import SourceSyntax.PrettyPrint
import Text.PrettyPrint as P

data Type = Lambda Type Type
          | Var String
          | Data String [Type]
          | EmptyRecord
          | Record (Map.Map String [Type]) Type
            deriving (Eq, Show, Data, Typeable)

fieldMap :: [(String,Type)] -> Map.Map String [Type]
fieldMap fields =
    foldl (\r (x,t) -> Map.insertWith (++) x [t] r) Map.empty fields

recordOf :: [(String,Type)] -> Type
recordOf fields = Record (fieldMap fields) EmptyRecord

listOf :: Type -> Type
listOf t = Data "_List" [t]

tupleOf :: [Type] -> Type
tupleOf ts = Data ("_Tuple" ++ show (length ts)) ts


instance Pretty Type where
  pretty tipe =
    case tipe of
      Lambda t1 t2 -> P.sep [ t, P.sep (map (P.text "->" <+>) ts) ]
        where t:ts = collectLambdas tipe
      Var x -> P.text x
      Data "_List" [t] -> P.brackets (pretty t)
      Data name tipes
          | Help.isTuple name -> P.parens . P.sep . P.punctuate P.comma $ map prettyParens tipes
          | otherwise -> P.hang (P.text name) 2 (P.sep $ map prettyParens tipes)
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