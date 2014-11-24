module Parse.Module (header, headerAndImports, getModuleName) where

import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate)
import Text.Parsec hiding (newline, spaces)

import Parse.Helpers
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
        (names, _) <- header
        return (intercalate "." names)

headerAndImports :: IParser Module.HeaderAndImports
headerAndImports =
    do optional freshLine
       (names, exports) <-
           option (["Main"], Var.openListing) (header `followedBy` freshLine)
       imports' <- commitToImports <|> return []
       return $ Module.HeaderAndImports names exports imports'
    where
      commitToImports =
          do  try (lookAhead $ reserved "import")
              imports `followedBy` freshLine

header :: IParser ([String], Var.Listing Var.Value)
header = do
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
          Just ctors -> return (Var.Union name ctors)
