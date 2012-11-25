
module Parse.Modules (moduleDef, imports) where

import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate)
import Text.Parsec hiding (newline,spaces)

import Ast
import Parse.Library

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
import' = do
  reserved "import"
  whitespace
  name <- intercalate "." <$> dotSep1 capVar
  method <- option (Hiding []) $ try (whitespace >>
                                      (as' <|> hiding' <|> importing'))
  return (name, method)


as' :: IParser ImportMethod
as' = reserved "as" >> whitespace >> As <$> capVar <?> "alias for module"

hiding' :: IParser ImportMethod
hiding' = reserved "hiding" >> whitespace >>
          Hiding <$> varList <?> "listing of hidden values"

importing' :: IParser ImportMethod
importing' = Importing <$> varList <?> "listing of imported values (x,y,z)"