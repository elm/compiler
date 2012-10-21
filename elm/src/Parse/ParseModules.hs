
module ParseModules (moduleDef, imports) where

import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate)
import Text.Parsec hiding (newline,spaces)

import Ast
import ParseLib

moduleDef :: (Monad m) => ParsecT [Char] u m ([String], [String])
moduleDef = do
  try (reserved "module")
  whitespace <?> "name of module"
  names <- dotSep1 capVar
  whitespace
  exports <- option [] . parens $ commaSep var
  whitespace <?> "reserved word 'where'"
  reserved "where"
  return (names, exports)

imports :: (Monad m) => ParsecT [Char] u m Imports
imports = option [] ((:) <$> import' <*> many (try (freshLine >> import')))


import' :: (Monad m) => ParsecT [Char] u m (String, ImportMethod)
import' = do
  reserved "import"
  whitespace
  name <- intercalate "." <$> dotSep1 capVar
  method <- option (Hiding []) $ try (whitespace >>
                                      (as' <|> hiding' <|> importing'))
  return (name, method)


as' :: (Monad m) => ParsecT [Char] u m ImportMethod
as' = reserved "as" >> whitespace >> As <$> capVar <?> "alias for module"

hiding' :: (Monad m) => ParsecT [Char] u m ImportMethod
hiding' = reserved "hiding" >> whitespace >>
          Hiding <$> parens (commaSep1 var) <?> "listing of hidden values"

importing' :: (Monad m) => ParsecT [Char] u m ImportMethod
importing' = Importing <$> parens (commaSep1 var) <?> "listing of imported values (x,y,z)"