module ParseTypes where

import Ast
import Control.Applicative ((<$>),(<*>))
import Control.Monad (liftM)
import Data.Char (isUpper,isLower)
import Data.Maybe (fromMaybe)
import Data.List (lookup)
import Text.Parsec

import ParseLib
import Types hiding (string,parens)
import Guid

data ParseType = VarPT String
               | LambdaPT ParseType ParseType
               | ADTPT String [ParseType]

listPT t = ADTPT "List" [t]
tuplePT ts = ADTPT ("Tuple" ++ show (length ts)) ts

typeVar :: (Monad m) => ParsecT [Char] u m ParseType
typeVar = VarPT <$> lowVar <?> "type variable"

typeList :: (Monad m) => ParsecT [Char] u m ParseType
typeList  = listPT <$> braces typeExpr

typeTuple :: (Monad m) => ParsecT [Char] u m ParseType
typeTuple = do ts <- parens (commaSep typeExpr)
               return $ case ts of { [t] -> t ; _ -> tuplePT ts }

typeUnambiguous :: (Monad m) => ParsecT [Char] u m ParseType
typeUnambiguous = typeList <|> typeTuple

typeSimple :: (Monad m) => ParsecT [Char] u m ParseType
typeSimple = VarPT <$> var

typeApp :: (Monad m) => ParsecT [Char] u m ParseType
typeApp = do name <- capVar <?> "type constructor"
             args <- spacePrefix (typeUnambiguous <|> typeSimple)
             return $ case args of
                        [] -> VarPT name
                        _  -> ADTPT name args

typeExpr :: (Monad m) => ParsecT [Char] u m ParseType
typeExpr = do
  t1 <- typeVar <|> typeApp <|> typeUnambiguous
  whitespace ; arr <- optionMaybe arrow ; whitespace
  case arr of Just _  -> LambdaPT t1 <$> typeExpr
              Nothing -> return t1

typeConstructor :: (Monad m) => ParsecT [Char] u m (String, [ParseType])
typeConstructor = (,) <$> (capVar <?> "another type constructor") <*> spacePrefix (typeSimple <|> typeUnambiguous)

datatype :: (Monad m) => ParsecT [Char] u m ([String], [Expr], GuidCounter [Type])
datatype = do
  reserved "data" <?> "datatype definition (data T = A | B | ...)"
  forcedWS ; name <- capVar <?> "name of data-type" ; args <- spacePrefix lowVar
  whitespace ; string "=" ; whitespace
  tcs <- pipeSep1 typeConstructor
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

toForeignType (LambdaPT t1 t2) =
    fail $ "Elm's JavaScript event interface does not yet handle functions. " ++
           "Only simple values can be imported and exported in this release."
    --LambdaT <$> toForeignType t1 <*> toForeignType t2
toForeignType (ADTPT name args)
    | isJsStructure name =  ADT name <$> mapM toForeignType args
    | otherwise =
        Left $ "'" ++ name ++ "' is not an exportable type " ++
               "constructor. Only 'JSArray' and 'JSTupleN' are exportable."

toForeignType (VarPT x@(c:_))
    | isLower c =
        Left "All exported types must be concrete types (JSNumber, JSString, etc.)"
    | x `elem` ["JSString","JSNumber","JSElement","JSBool"] = Right (ADT x [])
    | otherwise = Left $ "'" ++ x ++ "' is not an exportable type. Only JSTypes are exportable."

isJsStructure name = name == "JSArray" || isTuple
    where isTuple = "JSTuple" == take 7 name && name `elem` map show [2..5]