module Parse.Type where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (liftM,mapM)
import Data.Char (isLower)
import Data.List (lookup,intercalate)
import Text.Parsec
import Text.Parsec.Indent

import SourceSyntax.Location as Located
import SourceSyntax.Expression
import SourceSyntax.Declaration
import Parse.Helpers
import Types.Types hiding (parens,string)
import Unique

data ParseType = VarPT String
               | LambdaPT ParseType ParseType
               | ADTPT String [ParseType]
               | RecordPT (Maybe ParseType) [(String,ParseType)]
                 deriving (Show)

listPT t = ADTPT "List" [t]
tuplePT ts = ADTPT ("Tuple" ++ show (length ts)) ts

typeVar :: IParser ParseType
typeVar = VarPT <$> lowVar <?> "type variable"

typeList :: IParser ParseType
typeList  = listPT <$> braces typeExpr

typeTuple :: IParser ParseType
typeTuple = do ts <- parens (commaSep typeExpr)
               return $ case ts of { [t] -> t ; _ -> tuplePT ts }

typeRecord :: IParser ParseType
typeRecord = brackets (RecordPT <$> extend <*> fields)
  where extend = optionMaybe . try $ do
                   t <- typeVar
                   whitespace >> string "|" >> whitespace
                   return t
        fields = commaSep $ do
                   lbl <- rLabel
                   whitespace >> hasType >> whitespace
                   (,) lbl <$> typeExpr

typeUnambiguous :: IParser ParseType
typeUnambiguous = typeList <|> typeTuple <|> typeRecord

typeSimple :: IParser ParseType
typeSimple = VarPT <$> var

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
  case arr of
    Just _  -> LambdaPT t1 <$> typeExpr
    Nothing -> return t1

typeConstructor :: IParser (String, [ParseType])
typeConstructor = (,) <$> (capVar <?> "another type constructor")
                      <*> spacePrefix (typeSimple <|> typeUnambiguous)

typeAlias :: IParser [Declaration t v]
typeAlias = do
  start <- getPosition
  reserved "type" <?> "type alias (type Point = {x:Int, y:Int})"
  forcedWS
  alias <- capVar
  args  <- spacePrefix lowVar
  whitespace ; string "=" ; whitespace
  let n = length args
  tipe <- typeExpr
  end <- getPosition
  case toTypeWith alias (zip args [1..n]) tipe of
    Left msg -> fail msg
    Right t -> return (TypeAlias alias [1..n] t : ctor)
        where ctor = case tipe of
                       RecordPT extension kvs ->
                           [] --toConstructor start end alias extension kvs]
                       _ -> []
{--
toConstructor start end alias Nothing kvs =
    Definition (Def alias args (loc (Record rec)))
  where
    loc = Located.at start end
    args = map fst kvs
    rec = map (\a -> (a, loc (Var a))) args

toConstructor start end alias (Just _) kvs =
    Definition (Def alias (args++["_ext_"]) (loc rec))
  where
    loc = Located.at start end
    args = map fst kvs
    rec = foldl insert (Var "_ext_") (zip args (map (loc . Var) args))
    insert e (k,v) = Insert (loc e) k v
--}
annotation :: IParser (Def t v)
annotation = TypeAnnotation <$> try start <*> (toType <$> typeExpr)
    where start = do v <- lowVar <|> parens symOp
                     whitespace ; hasType ; whitespace ; return v

datatype :: IParser (Declaration t v)
datatype = do
  reserved "data" <?> "datatype definition (data T = A | B | ...)"
  forcedWS ; name <- capVar <?> "name of data-type" ; args <- spacePrefix lowVar
  whitespace ; string "=" ; whitespace
  tcs <- pipeSep1 typeConstructor
  case toDatatype name args tcs of
    Right dt -> return dt
    Left msg -> fail msg

beta = liftM VarT guid

toType :: ParseType -> Type
toType pt =
  let frees :: ParseType -> [String]
      frees pt = case pt of
                   LambdaPT a b  -> frees a ++ frees b
                   ADTPT _ ts    -> concatMap frees ts
                   RecordPT t fs -> maybe [] frees t ++ concatMap (frees . snd) fs
                   VarPT (c:cs) | isLower c -> [c:cs]
                                | otherwise -> []
  in  case toTypeWith "" (zip (frees pt) [1..]) pt of
        Right t -> t
        Left  _ -> VarT 0

toTypeWith :: String -> [(String,X)] -> ParseType -> Either String Type
toTypeWith name tvarDict pt =
  let msg x = "Type variable '" ++ x ++ "' is unbound in type '" ++ name ++ "'."
      toT = toTypeWith name tvarDict
  in  case pt of
        LambdaPT t1 t2  -> (==>) <$> toT t1 <*> toT t2
        ADTPT name args -> ADT name <$> mapM toT args
        RecordPT t fs   -> do fs' <- mapM (\(x,pt) -> (,) x <$> toT pt) fs
                              ext <- maybe (return EmptyRecord) toT t
                              return (RecordT (recordT fs') ext)
        VarPT x@(c:_)
            | not (isLower c) -> return $ ADT x []
            | otherwise -> VarT <$> case lookup x tvarDict of
                                      Just v -> Right v
                                      Nothing -> Left (msg x)


toDatatype name args tcs = Datatype name [1..n] <$> mapM toC tcs
    where n = length args
          toType = toTypeWith name (zip args [1..n])
          toC (name,pt) = (,) name <$> mapM toType pt


toForeignType (LambdaPT t1 t2) =
    fail $ "Elm's JavaScript event interface does not yet handle functions. " ++
           "Only simple values can be imported and exported in this release."

toForeignType (ADTPT "JSArray" args) =
    ADT "JSArray" <$> mapM toForeignType args

toForeignType (ADTPT name _) =
    Left $ "'" ++ name ++ "' is not an exportable type " ++
             "constructor. Only 'JSArray' is exportable."

toForeignType (VarPT x@(c:_))
    | x `elem` jsTypes = Right (ADT x [])
    | isLower c =
        Left $ "All exported types must be concrete types." ++ msg
    | otherwise =
        Left $ "'" ++ x ++ "' is not an exportable type." ++ msg
  where
    msg = " The following types are exportable: " ++ intercalate ", " jsTypes
    jsTypes = ["JSString","JSNumber","JSDomNode","JSBool","JSObject"]