{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Module (moduleDecl, header, getModuleName) where

import Text.Parsec hiding (newline, spaces)

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
          (_, names, _) <- moduleDecl
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
      (tag, names, exports) <-
        option
          (Module.None, ["Main"], Var.openListing)
          (moduleDecl `followedBy` freshLine)

      docs <-
        choice
          [ addLocation (Just <$> docComment) `followedBy` freshLine
          , addLocation (return Nothing)
          ]

      imports' <- imports

      return (Module.Header tag names docs exports imports')


moduleDecl :: IParser (Module.Tag, [String], Var.Listing (A.Located Var.Value))
moduleDecl =
  expecting "a module declaration" $
  do
      kind <-
        choice
          [ do  try (reserved "module")
                return Module.None
          , do  start <- getMyPosition
                try (reserved "effect")
                whitespace
                reserved "module"
                end <- getMyPosition
                return $ Module.Effect (R.Region start end)
          , do  start <- getMyPosition
                reserved "foreign"
                whitespace
                reserved "effect"
                whitespace
                reserved "module"
                end <- getMyPosition
                return $ Module.Foreign (R.Region start end)
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
      Var.Value <$> (lowVar <|> parens symOp)

    tipe =
      do  name <- capVar
          maybeCtors <- optionMaybe (listing capVar)
          case maybeCtors of
            Nothing -> return (Var.Alias name)
            Just ctors -> return (Var.Union name ctors)
