
module Parse.Declaration where

import Control.Applicative ((<$>), (<*>))
import qualified Data.List as List
import qualified Data.Set as Set
import Text.Parsec hiding (newline,spaces)
import Text.Parsec.Indent
import qualified Text.Pandoc as Pan

import Parse.Helpers
import qualified Parse.Expression as Expr
import qualified SourceSyntax.Type as T
import qualified Parse.Type as Type
import SourceSyntax.Declaration (Declaration(..), Assoc(..))

import Unique

declaration :: IParser (Declaration t v)
declaration = alias <|> datatype <|> infixDecl <|> foreignDef <|> definition

definition :: IParser (Declaration t v)
definition = Definition <$> Expr.def

alias :: IParser (Declaration t v)
alias = do
  reserved "type" <?> "type alias (type Point = {x:Int, y:Int})"
  forcedWS
  alias <- capVar
  args  <- spacePrefix lowVar
  whitespace ; string "=" ; whitespace
  tipe <- Type.expr
  return (TypeAlias alias args tipe)

datatype :: IParser (Declaration t v)
datatype = do
  reserved "data" <?> "datatype definition (data T = A | B | ...)"
  forcedWS
  name <- capVar <?> "name of data-type"
  args <- spacePrefix lowVar
  whitespace ; string "=" ; whitespace
  tcs <- pipeSep1 Type.constructor
  return $ Datatype name args tcs


infixDecl :: IParser (Declaration t v)
infixDecl = do
  assoc <- choice [ reserved "infixl" >> return L
                  , reserved "infix"  >> return N
                  , reserved "infixr" >> return R ]
  whitespace
  n <- digit
  forcedWS
  Fixity assoc (read [n]) <$> anyOp


foreignDef :: IParser (Declaration t v)
foreignDef = do
  try (reserved "foreign")
  whitespace
  importEvent <|> exportEvent

exportEvent :: IParser (Declaration t v)
exportEvent = do
  try (reserved "export") >> whitespace >> reserved "jsevent" >> whitespace
  eventName <- jsVar
  whitespace
  elmVar <- lowVar
  whitespace ; hasType ; whitespace
  tipe <- Type.expr
  case tipe of
    T.Data "Signal" [t] ->
        case isExportable t of
          Nothing -> return (ExportEvent eventName elmVar tipe)
          Just err -> fail err
    _ -> fail "When importing foreign events, the imported value must have type Signal."

importEvent :: IParser (Declaration t v)
importEvent = do
  try (reserved "import") >> whitespace >> reserved "jsevent" >> whitespace
  eventName <- jsVar
  whitespace
  baseValue <- Expr.term <?> "Base case for imported signal (signals cannot be undefined)"
  whitespace
  elmVar  <- lowVar <?> "Name of imported signal"
  whitespace ; hasType ; whitespace
  tipe <- Type.expr
  case tipe of
    T.Data "Signal" [t] ->
        case isExportable t of
          Nothing -> return (ImportEvent eventName baseValue elmVar tipe)
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
