module Parse.Module (moduleDef, getModuleName, imports) where

import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate)
import Text.Parsec hiding (newline,spaces)

import Parse.Helpers
import AST.Module (ImportMethod(..))
import AST.Variable (Listing(..), Value(..), openListing)

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
    method = as' <|> importing'

    as' :: IParser ImportMethod
    as' = do
      try (whitespace >> reserved "as")
      whitespace
      As <$> capVar <?> "alias for module"

    importing' :: IParser ImportMethod
    importing' = Open <$> listing value

listing :: IParser a -> IParser (Listing a)
listing item =
  do try (whitespace >> char '(')
     whitespace
     listing <- choice [ const openListing <$> string ".."
                       , Listing <$> commaSep1 item <*> return False
                       ] <?> "listing of values (x,y,z)"
     whitespace
     char ')'
     return listing

value :: IParser Value
value = val <|> tipe
    where
      val = Value <$> (lowVar <|> parens symOp)

      tipe = do
        name <- capVar
        maybeCtors <- optionMaybe (listing capVar)
        case maybeCtors of
          Nothing -> return (Alias name)
          Just ctors -> return (ADT name ctors)
