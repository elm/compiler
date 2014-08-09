module Parse.Module (moduleDef, getModuleName, imports) where

import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate)
import Text.Parsec ((<|>), (<?>), char, choice, many, option, optional, optionMaybe, string, try)

import Parse.Helpers (IParser, capVar, commaSep1, dotSep1, freshLine, lowVar, iParse, parens, reserved, symOp, whitespace)
import qualified AST.Module as Module
import qualified AST.Variable as Var

getModuleName :: String -> Maybe String
getModuleName source =
    case iParse getModuleName source of
      Right name -> Just name
      Left _     -> Nothing
    where
      getModuleName = do
        optional freshLine
        (names, _) <- moduleDef
        return (intercalate "." names)

moduleDef :: IParser ([String], Var.Listing Var.Value)
moduleDef = do
  try (reserved "module")
  whitespace
  names <- dotSep1 capVar <?> "name of module"
  whitespace
  exports <- option Var.openListing (listing value)
  whitespace <?> "reserved word 'where'"
  reserved "where"
  return (names, exports)

imports :: IParser [(Module.Name, Module.ImportMethod)]
imports = option [] ((:) <$> import' <*> many (try (freshLine >> import')))

import' :: IParser (Module.Name, Module.ImportMethod)
import' =
  do reserved "import"
     whitespace
     names <- dotSep1 capVar
     (,) names <$> option (Module.As (intercalate "." names)) method
  where
    method :: IParser Module.ImportMethod
    method = as' <|> importing'

    as' :: IParser Module.ImportMethod
    as' = do
      try (whitespace >> reserved "as")
      whitespace
      Module.As <$> capVar <?> "alias for module"

    importing' :: IParser Module.ImportMethod
    importing' = Module.Open <$> listing value

listing :: IParser a -> IParser (Var.Listing a)
listing item =
  do try (whitespace >> char '(')
     whitespace
     listing <- choice [ const Var.openListing <$> string ".."
                       , Var.Listing <$> commaSep1 item <*> return False
                       ] <?> "listing of values (x,y,z)"
     whitespace
     char ')'
     return listing

value :: IParser Var.Value
value = val <|> tipe
    where
      val = Var.Value <$> (lowVar <|> parens symOp)

      tipe = do
        name <- capVar
        maybeCtors <- optionMaybe (listing capVar)
        case maybeCtors of
          Nothing -> return (Var.Alias name)
          Just ctors -> return (Var.ADT name ctors)
