
module Parse.Declaration where

import Control.Applicative ((<$>), (<*>))
import qualified Data.List as List
import qualified Data.Set as Set
import Text.Parsec hiding (newline,spaces)
import Text.Parsec.Indent

import Parse.Helpers
import qualified Parse.Expression as Expr
import qualified SourceSyntax.Type as T
import qualified Parse.Type as Type
import qualified SourceSyntax.Declaration as D


declaration :: IParser (D.Declaration t v)
declaration = alias <|> datatype <|> infixDecl <|> foreignDef <|> definition

definition :: IParser (D.Declaration t v)
definition = D.Definition <$> Expr.def

alias :: IParser (D.Declaration t v)
alias = do
  reserved "type" <?> "type alias (type Point = {x:Int, y:Int})"
  forcedWS
  alias <- capVar
  args  <- spacePrefix lowVar
  padded equals
  tipe <- Type.expr
  json <- option [] $ do
            padded (reserved "deriving")
            string "Json"
            return [D.Json]
  return (D.TypeAlias alias args tipe json)

datatype :: IParser (D.Declaration t v)
datatype = do
  reserved "data" <?> "datatype definition (data T = A | B | ...)"
  forcedWS
  name <- capVar <?> "name of data-type"
  args <- spacePrefix lowVar
  padded equals
  tcs <- pipeSep1 Type.constructor
  return $ D.Datatype name args tcs


infixDecl :: IParser (D.Declaration t v)
infixDecl = do
  assoc <- choice [ reserved "infixl" >> return D.L
                  , reserved "infix"  >> return D.N
                  , reserved "infixr" >> return D.R ]
  forcedWS
  n <- digit
  forcedWS
  D.Fixity assoc (read [n]) <$> anyOp


foreignDef :: IParser (D.Declaration t v)
foreignDef = do
  try (reserved "foreign")
  whitespace
  importEvent <|> exportEvent

exportEvent :: IParser (D.Declaration t v)
exportEvent = do
  try (reserved "export") >> padded (reserved "jsevent")
  eventName <- jsVar
  whitespace
  elmVar <- lowVar
  padded hasType
  tipe <- Type.expr
  case tipe of
    T.Data "Signal" [t] ->
        case isExportable t of
          Nothing -> return (D.ExportEvent eventName elmVar tipe)
          Just err -> fail err
    _ -> fail "When importing foreign events, the imported value must have type Signal."

importEvent :: IParser (D.Declaration t v)
importEvent = do
  try (reserved "import") >> padded (reserved "jsevent")
  eventName <- jsVar
  baseValue <- padded Expr.term
               <?> "Base case for imported signal (signals cannot be undefined)"
  elmVar  <- lowVar <?> "Name of imported signal"
  padded hasType
  tipe <- Type.expr
  case tipe of
    T.Data "Signal" [t] ->
        case isExportable t of
          Nothing -> return (D.ImportEvent eventName baseValue elmVar tipe)
          Just err -> fail err
    _ -> fail "When importing foreign events, the imported value must have type Signal."

jsVar :: IParser String
jsVar = betwixt '"' '"' $ do
  v <- (:) <$> (letter <|> char '_') <*> many (alphaNum <|> char '_')
  if Set.notMember v jsReserveds then return v else
      fail $ "'" ++ v ++
          "' is not a good name for a importing or exporting JS values."

isExportable tipe =
  case tipe of
    T.Lambda _ _ ->
        Just $ "Elm's JavaScript event interface does not yet handle functions. " ++
               "Only simple values can be imported and exported in this release."

    T.Data "JSArray" [t] -> isExportable t

    T.Data name []
        | any (`List.isSuffixOf` name) jsTypes -> Nothing
        | otherwise -> Just $ "'" ++ name ++ "' is not an exportable type." ++ msg

    T.Data name _ ->
        Just $ "'" ++ name ++ "' is not an exportable type " ++
               "constructor. Only 'JSArray' is an exportable container."

    T.Var _ -> Just $ "Cannot export type variables." ++ msg
  where
    msg = " The following types are exportable: " ++ List.intercalate ", " jsTypes
    jsTypes = ["JSString","JSNumber","JSDomNode","JSBool","JSObject"]
