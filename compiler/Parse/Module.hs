module Parse.Module (moduleDef, getModuleName, imports) where

import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate)
import Text.Parsec hiding (newline,spaces)

import Parse.Helpers
import AST.Module (ImportMethod(..))
import AST.Variable (Listing(..), Value(..))

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

moduleDef :: IParser ([String], Listing Value)
moduleDef = do
  try (reserved "module")
  whitespace
  names <- dotSep1 capVar <?> "name of module"
  whitespace
  exports <- option (Listing [] True) (listing value)
  whitespace <?> "reserved word 'where'"
  reserved "where"
  return (names, exports)

imports :: IParser [(String, ImportMethod)]
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
    importing' = Open <$> listing value

listing :: IParser a -> IParser (Listing a)
listing item = 
    parens (choice [ const (Listing [] True) <$> string ".."
                   , Listing <$> commaSep1 item <*> return False
                   ] <?> "listing of values (x,y,z)")

value :: IParser Value
value = choice [ Value <$> (lowVar <|> parens symOp)
               , ADT <$> capVar <*> listing capVar
               ]

