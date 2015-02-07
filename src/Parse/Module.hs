module Parse.Module (header, headerAndImports, getModuleName) where

import Control.Applicative ((<$>), (<*>))
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
    getModuleName =
      do  optional freshLine
          (names, _) <- header
          return (Module.nameToString names)


headerAndImports :: IParser Module.HeaderAndImports
headerAndImports =
  do  optional freshLine
      (names, exports) <-
          option (["Main"], Var.openListing) (header `followedBy` freshLine)
      imports' <- imports
      return $ Module.HeaderAndImports names exports imports'


header :: IParser ([String], Var.Listing Var.Value)
header =
  do  try (reserved "module")
      whitespace
      names <- dotSep1 capVar <?> "name of module"
      whitespace
      exports <- option Var.openListing (listing value)
      whitespace <?> "reserved word 'where'"
      reserved "where"
      return (names, exports)


imports :: IParser [(Module.Name, Module.ImportMethod)]
imports =
  many (import' `followedBy` freshLine)


import' :: IParser (Module.Name, Module.ImportMethod)
import' =
  do  try (reserved "import")
      whitespace
      names <- dotSep1 capVar
      (,) names <$> method (Module.nameToString names)
  where
    method :: String -> IParser Module.ImportMethod
    method defaultAlias =
      Module.ImportMethod
          <$> option (Just defaultAlias) (Just <$> as')
          <*> option Var.closedListing exposing

    as' :: IParser String
    as' =
      do  try (whitespace >> reserved "as")
          whitespace
          capVar <?> "alias for module"

    exposing :: IParser (Var.Listing Var.Value)
    exposing =
      do  try (whitespace >> reserved "exposing")
          whitespace
          listing value


listing :: IParser a -> IParser (Var.Listing a)
listing item =
  do  try (whitespace >> char '(')
      whitespace
      listing <-
          choice
            [ const Var.openListing <$> string ".."
            , Var.Listing <$> commaSep1 item <*> return False
            ] <?> "listing of values (x,y,z)"
      whitespace
      char ')'
      return listing


value :: IParser Var.Value
value =
    val <|> tipe
  where
    val =
      Var.Value <$> (lowVar <|> parens symOp)

    tipe =
      do  name <- capVar
          maybeCtors <- optionMaybe (listing capVar)
          case maybeCtors of
            Nothing -> return (Var.Alias name)
            Just ctors -> return (Var.Union name ctors)
