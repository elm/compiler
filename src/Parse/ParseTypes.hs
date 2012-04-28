module ParseTypes where

import Ast
import Combinators
import Data.Char (isUpper)
import ParserLib
import Tokens
import Types
import Control.Monad (liftM)

typeVar = do whitespace; t <- item
             case t of
               ID "Int" -> return IntT
               ID "String" -> return StringT
               ID "Char" -> return CharT
               ID "Bool" -> return BoolT
               ID (v:vs) -> return (if isUpper v then ADT (v:vs) []
                                    else VarT (v:vs))
               _ -> zero

typeList = do t LBRACKET; ti <- typeExpr; t RBRACKET; return $ ADT "List" []
typeTuple = do { t LPAREN; ts <- sepBy (t COMMA) typeExpr; t RPAREN
                ; return $ case ts of { [t] -> t; _ -> ADT "Tuple" [] } }

typeUnamb = typeList +|+ typeTuple

typeTerm = typeApp +|+ typeUnamb

typeApp = do
  tipe <- typeVar
  case tipe of
    ADT name _ -> AppT name `liftM` star typeTerm
    _  -> return tipe

typeExpr = do t1 <- typeTerm
              arrow <- optional $ t ARROW
              case arrow of
                Just ARROW -> LambdaT t1 `liftM` typeTerm
                Nothing -> return t1

typeConstr = do
  name <- capVar
  args <- star (typeVar +|+ typeUnamb)
  return (Constructor name args)

constr (Constructor name args) =
    (name, foldr Lambda (Data name (map Var argNames)) argNames)
    where argNames = map (("arg"++) . show) [1..length args]

datatype = do
  t DATA
  adt <- capVar
  vs <- star var
  assign
  ts <- sepBy1 (opParser (=="|")) typeConstr
  -- (adt, foldr ForallT (ADT adt ts) vs)
  return (map constr ts)
