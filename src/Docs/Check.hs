module Docs.Check (check) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)

import qualified AST.Variable as Var
import qualified Docs.AST as Docs
import qualified Elm.Compiler.Type as Type
import Elm.Utils ((|>))
import qualified Reporting.Annotation as A
import qualified Reporting.Error as Error
import qualified Reporting.Error.Docs as E
import qualified Reporting.Helpers as Help (nearbyNames)
import qualified Reporting.Region as R
import qualified Reporting.Result as R
import Parse.Helpers
  ( Parser, addLocation, getPosition
  , spaces, checkSpace, whitespace, SPos(..)
  , capVar, lowVar, leftParen, rightParen, infixOp, runAt
  , chompUntilDocs, comma, oneOf
  )



-- CHECK DOCUMENTATION


check :: [Var.Value] -> A.Located (Maybe Docs.Centralized) -> R.Result () w Error.Error Docs.Checked
check exports (A.A region maybeDocs) =
  case maybeDocs of
    Nothing ->
      R.throw region (Error.Docs E.NoDocs)

    Just docs ->
      checkHelp region exports docs


checkHelp :: R.Region -> [Var.Value] -> Docs.Centralized -> R.Result () w Error.Error Docs.Checked
checkHelp region exports (Docs.Docs comment aliases types values) =
  let
    exportedUnions =
      map (\(tipe, Var.Listing tags _) -> (tipe, tags)) (Var.getUnions exports)
  in
    do  docNames <- parseNames region comment
        let docSet = Set.fromList (map A.drop docNames)
        R.mapError Error.Docs $
          (\() -> Docs.Docs comment)
            <$> checkModuleComment region exports docNames
            <*> checkCategory docSet (checkComment Docs.aliasComment) aliases
            <*> checkCategory docSet (checkUnion exportedUnions) types
            <*> checkCategory docSet checkValue values


type Result w a = R.Result () w E.Error a


checkCategory
  :: Set.Set Text
  -> (Text -> a -> Result w b)
  -> Map.Map Text a
  -> Result w (Map.Map Text b)
checkCategory docSet checker dict =
  dict
    |> Map.filterWithKey (\key _ -> Set.member key docSet)
    |> Map.mapWithKey checker
    |> traverse id


checkComment :: (a -> Maybe Text) -> Text -> A.Located a -> Result w (A.Located a)
checkComment getComment name (A.A region value) =
  case getComment value of
    Nothing ->
        R.throw region (E.NoComment name)

    Just _ ->
        R.ok (A.A region value)


checkUnion :: [(Text, [Text])] -> Text -> A.Located Docs.Union -> Result w (A.Located Docs.Union)
checkUnion exportedUnions name value =
  const
    <$> filterUnionTags exportedUnions name value
    <*> checkComment Docs.unionComment name value


filterUnionTags :: [(Text, [Text])] -> Text -> A.Located Docs.Union -> Result w (A.Located Docs.Union)
filterUnionTags exportedUnions name (A.A region union@(Docs.Union _ _ ctors)) =
  let
    exportedTags =
      maybe [] id (List.lookup name exportedUnions)
  in
    R.ok $ A.A region $ union
      { Docs.unionCases =
          filter (\(tag, _) -> elem tag exportedTags) ctors
      }



checkValue :: Text -> A.Located (Docs.Value (Maybe Type.Type)) -> Result w (A.Located (Docs.Value Type.Type))
checkValue name value =
  const
    <$> hasType name value
    <*> checkComment Docs.valueComment name value


hasType :: Text -> A.Located (Docs.Value (Maybe Type.Type)) -> Result w (A.Located (Docs.Value Type.Type))
hasType name (A.A region value) =
  case Docs.valueType value of
    Just tipe ->
        R.ok (A.A region (value { Docs.valueType = tipe }))

    Nothing ->
        R.throw region (E.NoType name)



-- CHECK MODULE COMMENT


checkModuleComment :: R.Region -> [Var.Value] -> [A.Located Text] -> Result w ()
checkModuleComment docRegion exports locatedDocNames =
  let
    exportNames =
      map valueName exports

    exportDict =
      Map.fromList (map (\name -> (name, ())) exportNames)

    docDict =
      Map.fromList (map (\(A.A region name) -> (name, region)) locatedDocNames)

    extraNameError (name, region) =
      R.throw region (E.OnlyInDocs name (Help.nearbyNames id name exportNames))

    docErrors =
      Map.difference docDict exportDict
        |> Map.toList
        |> traverse extraNameError

    namesOnlyInExports =
      Map.difference exportDict docDict
        |> Map.toList
        |> map fst

    exportErrors =
      if null namesOnlyInExports then
          R.ok ()
      else
          R.throw docRegion (E.OnlyInExports namesOnlyInExports)
  in
    (\_ _ -> ())
      <$> exportErrors
      <*> docErrors



-- PARSE DOC COMMENT


parseNames :: R.Region -> Text -> R.Result () w Error.Error [A.Located Text]
parseNames (R.Region (R.Position row col) _) comment =
  case runAt row (col + 3) (namesParser []) comment of
    Left (A.A region parseError) ->
      R.throw region (Error.Syntax parseError)

    Right names ->
      R.ok names



namesParser :: [A.Located Text] -> Parser [A.Located Text]
namesParser names =
  do  isDocs <- chompUntilDocs
      if isDocs then namesParserHelp names else return names


namesParserHelp :: [A.Located Text] -> Parser [A.Located Text]
namesParserHelp names =
  do  pos <- getPosition
      (SPos spos) <- whitespace
      if pos == spos
        then namesParser names
        else namesParser =<< chompNames names


chompNames :: [A.Located Text] -> Parser [A.Located Text]
chompNames names =
  do  name <- addLocation (oneOf [ lowVar, capVar, operator ])
      spos <- whitespace
      oneOf
        [ do  checkSpace spos
              comma
              spaces
              chompNames (name:names)
        , return (name:names)
        ]


operator :: Parser Text
operator =
  do  leftParen
      op <- infixOp
      rightParen
      return op


valueName :: Var.Value -> Text
valueName value =
  case value of
    Var.Value name -> name
    Var.Alias name -> name
    Var.Union name _ -> name
