module Parse.Module (moduleDecl, header, getModuleName) where

import Text.Parsec hiding (newline, spaces)

import Parse.Helpers
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import Reporting.Annotation as A


getModuleName :: String -> Maybe String
getModuleName source =
  let
    getModuleName =
      do  optional freshLine
          (_, names, _) <- moduleDecl
          return (ModuleName.toString names)
  in
    case iParse getModuleName source of
      Right name ->
        Just name

      Left _ ->
        Nothing


header :: IParser (Module.Header [Module.UserImport])
header =
  do  optional freshLine
      (kind, names, exports) <-
        option
          (Module.Normal, ["Main"], Var.openListing)
          (moduleDecl `followedBy` freshLine)

      docs <-
        choice
          [ addLocation (Just <$> docComment) `followedBy` freshLine
          , addLocation (return Nothing)
          ]

      imports' <- imports

      return (Module.Header kind names docs exports imports')


moduleDecl :: IParser (Module.Kind, [String], Var.Listing (A.Located Var.Value))
moduleDecl =
  expecting "a module declaration" $
  do
      kind <-
        choice
          [ do  try (reserved "module")
                return Module.Normal
          , do  try (reserved "effect")
                whitespace
                reserved "module"
                return Module.Effect
          , do  reserved "foreign"
                whitespace
                reserved "effect"
                whitespace
                reserved "module"
                return Module.Foreign
          ]

      whitespace
      names <- dotSep1 capVar <?> "the name of this module"

      whitespace
      exports <- option Var.openListing (listing (addLocation value))

      whitespace
      reserved "where"

      return (kind, names, exports)


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
      listing <-
          choice
            [ const Var.openListing <$> string ".."
            , Var.Listing <$> commaSep1 item <*> return False
            ]
      whitespace
      char ')'
      return listing


value :: IParser Var.Value
value =
    val <|> tipe <?> "a value or type to expose"
  where
    val =
      Var.Value <$> (lowVar <|> parens symOp)

    tipe =
      do  name <- capVar
          maybeCtors <- optionMaybe (listing capVar)
          case maybeCtors of
            Nothing -> return (Var.Alias name)
            Just ctors -> return (Var.Union name ctors)
