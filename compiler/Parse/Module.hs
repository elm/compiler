
module Parse.Module (moduleDef, imports) where

import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate)
import Text.Parsec hiding (newline,spaces)

import Parse.Helpers
import SourceSyntax.Module

varList ::  IParser [String]
varList = parens $ commaSep1 (var <|> parens symOp)

moduleDef :: IParser ([String], [String])
moduleDef = do
  try (reserved "module")
  whitespace
  names <- dotSep1 capVar <?> "name of module"
  whitespace
  exports <- option [] varList
  whitespace <?> "reserved word 'where'"
  reserved "where"
  return (names, exports)

imports :: IParser Imports
imports = option [] ((:) <$> import' <*> many (try (freshLine >> import')))

import' :: IParser (String, ImportMethod)
import' =
  do reserved "import"
     whitespace
     open <- optionMaybe (reserved "open")
     whitespace
     name <- intercalate "." <$> dotSep1 capVar
     case open of
       Just _ -> return (name, Hiding [])
       Nothing -> let how = try (whitespace >> (as' <|> importing'))
                  in  (,) name <$> option (Importing []) how
  where
    as' :: IParser ImportMethod
    as' = reserved "as" >> whitespace >> As <$> capVar <?> "alias for module"

    importing' :: IParser ImportMethod
    importing' = Importing <$> varList <?> "listing of imported values (x,y,z)"