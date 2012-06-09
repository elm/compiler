module ParseTypes where

import Ast
import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Data.Char (isUpper,isLower)
import Data.Maybe (fromMaybe)
import Data.List (lookup)
import Text.Parsec

import ParseLib
import Types
import Guid

data ParseType = VarPT String
               | LambdaPT ParseType ParseType
               | ADTPT String [ParseType]

listPT t = ADTPT "List" [t]
tuplePT ts = ADTPT ("Tuple" ++ show (length ts)) ts

typeVar :: (Monad m) => ParsecT [Char] u m ParseType
typeVar = VarPT <$> lowVar <?> "type variable"

typeList :: (Monad m) => ParsecT [Char] u m ParseType
typeList  = listPT <$> betwixtSpcs '[' ']' typeExpr

typeTuple :: (Monad m) => ParsecT [Char] u m ParseType
typeTuple = do ts <- betwixtSpcs '(' ')' (commaSep typeExpr)
               return $ case ts of { [t] -> t ; _ -> tuplePT ts }

typeUnambiguous :: (Monad m) => ParsecT [Char] u m ParseType
typeUnambiguous = typeList <|> typeTuple

typeSimple :: (Monad m) => ParsecT [Char] u m ParseType
typeSimple = VarPT <$> var

typeApp :: (Monad m) => ParsecT [Char] u m ParseType
typeApp = do name <- capVar
             args <- many (typeSimple <|> typeUnambiguous)
             return $ case args of
                        [] -> VarPT name
                        _  -> ADTPT name args

typeExpr :: (Monad m) => ParsecT [Char] u m ParseType
typeExpr = do
  whitespace
  t1 <- typeVar <|> typeApp <|> typeUnambiguous
  arrow <- optionMaybe arrow
  case arrow of Just _  -> LambdaPT t1 <$> typeExpr
                Nothing -> return t1

typeConstructor :: (Monad m) => ParsecT [Char] u m (String, [ParseType])
typeConstructor = do name <- capVar
                     args <- many (typeSimple <|> typeUnambiguous)
                     return $ (,) name args

datatype :: (Monad m) => ParsecT [Char] u m ([String], [Expr], GuidCounter [Type])
datatype = do
  symbol "data" <?> "datatype definition"
  name <- capVar ; args <- many lowVar ; symbol "="
  tcs <- typeConstructor `sepBy1` symbol "|"
  return $ (map fst tcs , map toFunc tcs , toTypes name args tcs)

beta = liftM VarT guid

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
