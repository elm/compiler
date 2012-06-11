
module ParseModules (moduleDef, imports) where

import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate)
import Text.Parsec hiding (newline,spaces)

import Ast
import ParseLib

moduleDef :: (Monad m) => ParsecT [Char] u m (String, [String])
moduleDef = do
  reserved "module"
  whitespace
  name <- capVar
  whitespace
  exports <- option [] . parens $ commaSep var
  whitespace
  reserved "where"
  return (name, exports)

imports :: (Monad m) => ParsecT [Char] u m Imports
imports =
    Imports <$> option [] ((:) <$> import' <*> many (try (freshLine >> import')))


import' :: (Monad m) => ParsecT [Char] u m (String, ImportMethod)
import' = do
  reserved "import"
  whitespace
  name <- intercalate "." <$> dotSep1 capVar
  method <- option (Importing []) $ try (whitespace >>
                                         (as' <|> hiding' <|> importing'))
  return (name, method)


as' :: (Monad m) => ParsecT [Char] u m ImportMethod
as' = reserved "as" >> whitespace >> As <$> capVar

hiding' :: (Monad m) => ParsecT [Char] u m ImportMethod
hiding' = reserved "hiding" >> whitespace >> Hiding <$> parens (commaSep1 var)

importing' :: (Monad m) => ParsecT [Char] u m ImportMethod
importing' = Importing <$> parens (commaSep1 var)