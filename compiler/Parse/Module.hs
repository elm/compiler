module Parse.Module (moduleDef, getModuleName, imports) where

import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate)
import Text.Parsec hiding (newline,spaces)

import Parse.Helpers
import SourceSyntax.Module (ImportMethod(..), Imports)

varList ::  IParser [String]
varList = commaSep1 (var <|> parens symOp)

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

moduleDef :: IParser ([String], [String])
moduleDef = do
  try (reserved "module")
  whitespace
  names <- dotSep1 capVar <?> "name of module"
  whitespace
  exports <- option [] (parens varList)
  whitespace <?> "reserved word 'where'"
  reserved "where"
  return (names, exports)

imports :: IParser Imports
imports = option [] ((:) <$> import' <*> many (try (freshLine >> import')))

import' :: IParser (String, ImportMethod)
import' =
  do reserved "import"
     whitespace
     name <- intercalate "." <$> dotSep1 capVar
     (,) name <$> option (As name) method
  where
    method :: IParser ImportMethod
    method = try $ do whitespace
                      as' <|> importing'

    as' :: IParser ImportMethod
    as' = do
      reserved "as"
      whitespace
      As <$> capVar <?> "alias for module"

    importing' :: IParser ImportMethod
    importing' =
        parens (choice [ const (Hiding []) <$> string ".."
                       , Importing <$> varList
                       ] <?> "listing of imported values (x,y,z)")
