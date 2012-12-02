module Parse.Types where

import Ast
import Control.Applicative ((<$>),(<*>))
import Control.Monad (liftM,mapM)
import Data.Char (isUpper,isLower)
import Data.Maybe (fromMaybe)
import Data.List (lookup)
import Text.Parsec
import Text.Parsec.Indent

import Parse.Library
import Types.Types hiding (string,parens)
import Guid

data ParseType = VarPT String
               | LambdaPT ParseType ParseType
               | ADTPT String [ParseType]

listPT t = ADTPT "List" [t]
tuplePT ts = ADTPT ("Tuple" ++ show (length ts)) ts

typeVar :: IParser ParseType
typeVar = VarPT <$> lowVar <?> "type variable"

typeList :: IParser ParseType
typeList  = listPT <$> braces typeExpr

typeTuple :: IParser ParseType
typeTuple = do ts <- parens (commaSep typeExpr)
               return $ case ts of { [t] -> t ; _ -> tuplePT ts }

typeUnambiguous :: IParser ParseType
typeUnambiguous = typeList <|> typeTuple

typeSimple :: IParser ParseType
typeSimple = dealias <$> var
    where dealias "String" = listPT (VarPT "Char")
          dealias "Time" = VarPT "Float"
          dealias v = VarPT v

typeApp :: IParser ParseType
typeApp = do name <- capVar <?> "type constructor"
             args <- spacePrefix (typeUnambiguous <|> typeSimple)
             return $ case args of
                        [] -> VarPT name
                        _  -> ADTPT name args

typeExpr :: IParser ParseType
typeExpr = do
  t1 <- typeVar <|> typeApp <|> typeUnambiguous
  whitespace ; arr <- optionMaybe arrow ; whitespace
  case arr of Just _  -> LambdaPT t1 <$> typeExpr
              Nothing -> return t1

typeConstructor :: IParser (String, [ParseType])
typeConstructor = (,) <$> (capVar <?> "another type constructor")
                      <*> spacePrefix (typeSimple <|> typeUnambiguous)

datatype :: IParser Statement
datatype = do
  reserved "data" <?> "datatype definition (data T = A | B | ...)"
  forcedWS ; name <- capVar <?> "name of data-type" ; args <- spacePrefix lowVar
  whitespace ; string "=" ; whitespace
  tcs <- pipeSep1 typeConstructor
  case toDatatype name args tcs of
    Right dt -> return dt
    Left msg -> fail msg

beta = liftM VarT guid

toDatatype name args tcs = Datatype name [1..n] <$> mapM toC tcs
    where n = length args
          tvarDict = zip args [1..n]
          toC (name,pt) = (,) name <$> mapM toT pt
          toT (LambdaPT t1 t2)  = (==>) <$> toT t1 <*> toT t2
          toT (ADTPT name args) = ADT name <$> mapM toT args
          toT (VarPT x@(c:_))
              | isLower c = VarT <$> case lookup x tvarDict of
                                       Just v -> Right v
                                       Nothing -> Left $ msg x
              | otherwise = return $ ADT x []
          msg x = "Type variable '" ++ x ++
                  "' is unbound in type constructor '" ++ name ++ "'."


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
    where isTuple = "JSTuple" == take 7 name && drop 7 name `elem` map show [2..5]