
module Parse.Modules (moduleDef, imports) where

import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate)
import Text.Parsec hiding (newline,spaces)

import Ast
import Parse.Library

varList ::  (Monad m) => ParsecT [Char] u m [String]
varList = parens $ commaSep1 (var <|> parens symOp)

moduleDef :: (Monad m) => ParsecT [Char] u m ([String], [String])
moduleDef = do
  try (reserved "module")
  whitespace
  names <- dotSep1 capVar <?> "name of module"
  whitespace
  exports <- option [] varList
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
          Hiding <$> varList <?> "listing of hidden values"

importing' :: (Monad m) => ParsecT [Char] u m ImportMethod
importing' = Importing <$> varList <?> "listing of imported values (x,y,z)"