module ParseTypes where

import Ast
import Combinators
import Data.Char (isUpper)
import ParserLib
import Tokens
import Types

type_var = do whitespace; t <- item
              case t of
                ID "Int" -> return IntT
                ID "String" -> return StringT
                ID "Char" -> return CharT
                ID "Bool" -> return BoolT
                ID (v:vs) -> return (if isUpper v then ADT (v:vs) []
                                     else VarT (v:vs))
                _ -> zero

type_list = do t LBRACKET; ti <- type_expr; t RBRACKET; return $ ADT "List" []
type_tuple = do { t LPAREN; ts <- sepBy (t COMMA) type_expr; t RPAREN
                ; return $ case ts of { [t] -> t; _ -> ADT "Tuple" [] } }

type_unamb = type_list +|+ type_tuple

type_term = type_app +|+ type_unamb

type_app = do
  tipe <- type_var
  case tipe of
    ADT name _ -> star type_term >>= return . AppT name
    _  -> return tipe

type_expr = do t1 <- type_term
               arrow <- optional $ t ARROW
               case arrow of
                 Just ARROW -> type_term >>= return . LambdaT t1
                 Nothing -> return t1

type_constr = do
  name <- cap_var
  args <- star (type_var +|+ type_unamb)
  return (Constructor name args)

constr (Constructor name args) =
    (name, foldr Lambda (Data name (map Var argNames)) argNames)
    where argNames = map (("arg"++) . show) [1..length args]

datatype = do
  t DATA
  adt <- cap_var
  vs <- star var
  assign
  ts <- sepBy1 (op_parser (=="|")) type_constr
  -- (adt, foldr ForallT (ADT adt ts) vs)
  return (map constr ts)