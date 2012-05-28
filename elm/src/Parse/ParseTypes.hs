module ParseTypes where

import Ast
import Combinators
import Data.Char (isUpper,isLower)
import Data.Maybe (fromMaybe)
import Data.List (lookup)
import ParserLib
import Tokens
import Types
import Guid
import Control.Monad (liftM)

data ParseType = VarPT String
               | LambdaPT ParseType ParseType
               | ADTPT String [ParseType]

listPT t = ADTPT "List" [t]
tuplePT ts = ADTPT ("Tuple" ++ show (length ts)) ts

typeVar = liftM VarPT lowVar
typeList  = do t LBRACKET; te <- typeExpr; t RBRACKET; return $ listPT te
typeTuple = do { t LPAREN; ts <- sepBy (t COMMA) typeExpr; t RPAREN
               ; return $ case ts of { [t] -> t ; _ -> tuplePT ts } }
typeUnambiguous = typeList +|+ typeTuple

typeSimple = liftM VarPT var
typeApp = do name <- capVar
             args <- star (typeSimple +|+ typeUnambiguous)
             return $ case args of
                        [] -> VarPT name
                        _  -> ADTPT name args
                     
typeExpr = do
  t1 <- typeVar +|+ typeApp +|+ typeUnambiguous
  arrow <- optional $ t ARROW
  case arrow of Just ARROW -> LambdaPT t1 `liftM` typeExpr
                Nothing -> return t1

typeConstructor = do name <- capVar
                     args <- star (typeSimple +|+ typeUnambiguous)
                     return $ (,) name args

datatype = do
  t DATA ; name <- capVar ; args <- star lowVar ; assign
  tcs <- sepBy1 (opParser (=="|")) typeConstructor
  return $ (map fst tcs , map toFunc tcs , toTypes name args tcs)

beta = VarT `liftM` guid

toFunc (name,args) = foldr Lambda (Data name $ map Var argNames) argNames
    where argNames = map (("a"++) . show) [1..length args]

toTypes name args constructors = do
  pairs <- mapM (\x -> (,) x `liftM` guid) args
  return $ map (toType pairs . ADT name $ map (VarT . snd) pairs) constructors

toType pairs outType (name,args) =
    foldr (==>) outType (map toT args)
    where toT (LambdaPT t1 t2)  = toT t1 ==> toT t2
          toT (ADTPT name args) = ADT name $ map toT args
          toT (VarPT x@(c:_))
              | isLower c = VarT . fromMaybe (-1) $ lookup x pairs
              | otherwise = case x of "Int" -> IntT
                                      "Number" -> IntT
                                      "String" -> StringT
                                      "Char" -> CharT
                                      "Bool" -> BoolT
                                      _ -> ADT x []
