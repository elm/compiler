{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Module (moduleDecl, header, getModuleName) where

import Text.Parsec hiding (newline, spaces)

import Parse.Binop (infixOp)
import Parse.Helpers
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


getModuleName :: String -> Maybe String
getModuleName source =
  let
    minimalParser =
      do  optional freshLine
          (ModuleDecl _ names _ _) <- moduleDecl
          return (ModuleName.toString names)
  in
    case iParse minimalParser source of
      Right name ->
        Just name

      Left _ ->
        Nothing


header :: IParser (Module.Header [Module.UserImport])
header =
  do  optional freshLine
      (ModuleDecl tag names exports settings) <-
        option
          (ModuleDecl Module.Normal ["Main"] Var.openListing Module.emptySettings)
          (moduleDecl `followedBy` freshLine)

      docs <-
        choice
          [ addLocation (Just <$> docComment) `followedBy` freshLine
          , addLocation (return Nothing)
          ]

      imports' <- imports

      return (Module.Header tag names exports settings docs imports')


data ModuleDecl =
  ModuleDecl
    { _tag :: Module.SourceTag
    , _name :: [String]
    , _exports :: Var.Listing (A.Located Var.Value)
    , _settings :: Module.SourceSettings
    }


moduleDecl :: IParser ModuleDecl
moduleDecl =
  expecting "a module declaration" $
  do
      tag <- parseTag
      whitespace

      names <- dotSep1 capVar <?> "the name of this module"
      whitespace

      settings <-
        case tag of
          Module.Effect _ ->
            parseSetting <* whitespace

          _ ->
            return Module.emptySettings

      reserved "exposing" <?> "something like `exposing (..)` which replaced `where` in 0.17"
      whitespace

      exports <- listing (addLocation value)

      return (ModuleDecl tag names exports settings)


parseTag :: IParser Module.SourceTag
parseTag =
  choice
    [
      do  try (reserved "module")
          return Module.Normal
    ,
      do  start <- getMyPosition
          try (reserved "effect")
          whitespace
          reserved "module"
          end <- getMyPosition
          return (Module.Effect (R.Region start end))
    ,
      do  start <- getMyPosition
          try (reserved "port")
          whitespace
          reserved "module"
          end <- getMyPosition
          return (Module.Port (R.Region start end))
    ]


parseSetting :: IParser Module.SourceSettings
parseSetting =
  do  try (reserved "where")
      whitespace
      addLocation $ brackets $ commaSep $
        do  name <- addLocation lowVar
            padded equals
            typeName <- addLocation capVar
            return (name, typeName)


imports :: IParser [Module.UserImport]
imports =
  many (import' `followedBy` freshLine)


import' :: IParser Module.UserImport
import' =
  expecting "an import" $
  addLocation $
  do  try (reserved "import")
      whitespace
      names <- dotSep1 capVar
      (,) names <$> method (ModuleName.toString names)
  where
    method :: String -> IParser Module.ImportMethod
    method defaultAlias =
      Module.ImportMethod
          <$> option (Just defaultAlias) (Just <$> as' defaultAlias)
          <*> option Var.closedListing exposing

    as' :: String -> IParser String
    as' moduleName =
      do  try (whitespace >> reserved "as")
          whitespace
          capVar <?> ("an alias for module `" ++ moduleName ++ "`")

    exposing :: IParser (Var.Listing Var.Value)
    exposing =
      do  try (whitespace >> reserved "exposing")
          whitespace
          listing value


listing :: IParser a -> IParser (Var.Listing a)
listing item =
  expecting "a listing of values and types to expose, like (..)" $
  do  try (whitespace >> char '(')
      whitespace
      actualListing <-
          choice
            [ const Var.openListing <$> string ".."
            , Var.Listing <$> commaSep1 item <*> return False
            ]
      whitespace
      char ')'
      return actualListing


value :: IParser Var.Value
value =
    val <|> tipe <?> "a value or type to expose"
  where
    val =
      Var.Value <$> (lowVar <|> parens infixOp)

    tipe =
      do  name <- capVar
          maybeCtors <- optionMaybe (listing capVar)
          case maybeCtors of
            Nothing -> return (Var.Alias name)
            Just ctors -> return (Var.Union name ctors)
