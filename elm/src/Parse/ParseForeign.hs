
module ParseForeign (foreignDefs) where

import Control.Applicative ((<$>), (<*>))
import Data.Either (partitionEithers)
import Text.Parsec hiding (newline,spaces)

import Ast
import ParseLib
import ParseExpr (term)
import ParseTypes
import Types (signalOf)


foreignDefs = do
  f  <- foreign --commitIf (reserved "foreign") foreign
  fs <- many (commitIf (freshLine >> reserved "foreign") (freshLine >> foreign))
  return . partitionEithers $ f:fs


foreign = do try (reserved "foreign") ; whitespace
             Left <$> importEvent <|> Right <$> exportEvent

exportEvent = do
  try (reserved "export" >> whitespace >> reserved "jsevent" >> whitespace)
  js   <- jsVar    ; whitespace
  elm  <- lowVar   ; whitespace
  string "::"      ; whitespace
  tipe <- typeExpr
  case tipe of
    ADTPT "Signal" [pt] ->
        either fail (return . (,,) js elm . signalOf) (toForeignType pt)
    _ -> fail "When exporting events, the exported value must be a Signal."

importEvent = do
  try (reserved "import" >> whitespace >> reserved "jsevent" >> whitespace)
  js   <- jsVar ; whitespace
  base <- term <?> "Base case for imported signal (signals cannot be undefined)"
  whitespace
  elm  <- lowVar <?> "Name of imported signal"
  whitespace ; string "::" ; whitespace
  tipe <- typeExpr
  case tipe of
    ADTPT "Signal" [pt] ->
        either fail (return . (,,,) js base elm . signalOf) (toForeignType pt)
    _ -> fail "When importing events, the imported value must be a Signal."


jsVar :: (Monad m) => ParsecT [Char] u m String
jsVar = betwixt '"' '"' $ do
  v <- (:) <$> (letter <|> char '_') <*> many (alphaNum <|> char '_')
  if v `notElem` jsReserveds then return v else
      fail $ "'" ++ v ++
          "' is not a good name for a importing or exporting JS values."

jsReserveds =
    [ "null", "undefined", "Nan", "Infinity", "true", "false", "eval"
    , "arguments", "int", "byte", "char", "goto", "long", "final", "float"
    , "short", "double", "native", "throws", "boolean", "abstract", "volatile"
    , "transient", "synchronized", "function", "break", "case", "catch"
    , "continue", "debugger", "default", "delete", "do", "else", "finally"
    , "for", "function", "if", "in", "instanceof", "new", "return", "switch"
    , "this", "throw", "try", "typeof", "var", "void", "while", "with", "class"
    , "const", "enum", "export", "extends", "import", "super", "implements"
    , "interface", "let", "package", "private", "protected", "public"
    , "static", "yield"
    ]