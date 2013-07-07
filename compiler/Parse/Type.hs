module Parse.Type where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (liftM,mapM)
import Data.Char (isLower)
import Data.List (lookup,intercalate)
import Text.Parsec
import Text.Parsec.Indent

import SourceSyntax.Location as Located
--import SourceSyntax.PrettyPrint
import SourceSyntax.Type as T
--import qualified SourceSyntax.Expression as Expr
--import qualified SourceSyntax.Declaration as Decl
import Parse.Helpers
import Unique

tvar :: IParser T.Type
tvar = T.Var <$> lowVar <?> "type variable"

list :: IParser T.Type
list = listOf <$> braces expr

tuple :: IParser T.Type
tuple = do ts <- parens (commaSep expr)
           return $ case ts of
                      [t] -> t
                      _   -> tupleOf ts

record :: IParser T.Type
record = brackets $ do
           ext <- extend
           fs <- fields
           return (T.Record (fieldMap fs) ext)
  where
    extend = option T.EmptyRecord . try $ do
               t <- tvar
               whitespace >> string "|" >> whitespace
               return t
    fields = commaSep $ do
               lbl <- rLabel
               whitespace >> hasType >> whitespace
               (,) lbl <$> expr

constructor0 :: IParser T.Type
constructor0 =
  do name <- capVar
     return (T.Data name [])

term :: IParser T.Type
term = list <|> tuple <|> record <|> tvar <|> constructor0

app :: IParser T.Type
app =
  do name <- capVar <?> "type constructor"
     args <- spacePrefix term
     return (T.Data name args)

expr :: IParser T.Type
expr =
  do t1 <- app <|> term
     whitespace
     arr <- optionMaybe arrow
     whitespace
     case arr of
       Just _  -> T.Lambda t1 <$> expr
       Nothing -> return t1

constructor :: IParser (String, [T.Type])
constructor = (,) <$> (capVar <?> "another type constructor")
                  <*> spacePrefix term

{--
datatype :: IParser (Declaration t v)
datatype = do
  reserved "data" <?> "datatype definition (data T = A | B | ...)"
  forcedWS ; name <- capVar <?> "name of data-type" ; args <- spacePrefix lowVar
  whitespace ; string "=" ; whitespace
  tcs <- pipeSep1 typeConstructor
  case toDatatype name args tcs of
    Right dt -> return dt
    Left msg -> fail msg

beta = liftM T.Var guid

toType :: T.Type -> Type
toType pt =
  let frees :: T.Type -> [String]
      frees pt = case pt of
                   LambdaPT a b  -> frees a ++ frees b
                   ADTPT _ ts    -> concatMap frees ts
                   RecordPT t fs -> maybe [] frees t ++ concatMap (frees . snd) fs
                   VarPT (c:cs) | isLower c -> [c:cs]
                                | otherwise -> []
  in  case toTypeWith "" (zip (frees pt) ['a'..]) pt of
        Right t -> t
        Left  _ -> T.Var 0

toTypeWith :: String -> [(String,String)] -> T.Type -> Either String Type
toTypeWith name tvarDict pt =
  let msg x = "Type variable '" ++ x ++ "' is unbound in type '" ++ name ++ "'."
      toT = toTypeWith name tvarDict
  in  case pt of
        LambdaPT t1 t2  -> T.Lambda <$> toT t1 <*> toT t2
        ADTPT name args -> T.Data name <$> mapM toT args
        RecordPT t fs   -> do fs' <- mapM (\(x,pt) -> (,) x <$> toT pt) fs
                              ext <- maybe (return T.EmptyRecord) toT t
                              return (T.Record (T.record fs') ext)
        VarPT x@(c:_)
            | not (isLower c) -> return $ T.Data x []
            | otherwise -> T.Var <$> case lookup x tvarDict of
                                       Just v -> Right v
                                       Nothing -> Left (msg x)


toDatatype name args tcs = Datatype name [1..n] <$> mapM toC tcs
    where n = length args
          toType = toTypeWith name (zip args (map (:[]) ['a'..]))
          toC (name,pt) = (,) name <$> mapM toType pt


toForeignType (LambdaPT t1 t2) =
    fail $ "Elm's JavaScript event interface does not yet handle functions. " ++
           "Only simple values can be imported and exported in this release."

toForeignType (ADTPT "JSArray" args) =
    T.Data "JSArray" <$> mapM toForeignType args

toForeignType (ADTPT name _) =
    Left $ "'" ++ name ++ "' is not an exportable type " ++
             "constructor. Only 'JSArray' is exportable."

toForeignType (VarPT x@(c:_))
    | x `elem` jsTypes = Right (T.Data x [])
    | isLower c =
        Left $ "All exported types must be concrete types." ++ msg
    | otherwise =
        Left $ "'" ++ x ++ "' is not an exportable type." ++ msg
  where
    msg = " The following types are exportable: " ++ intercalate ", " jsTypes
    jsTypes = ["JSString","JSNumber","JSDomNode","JSBool","JSObject"]
--}