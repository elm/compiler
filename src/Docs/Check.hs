{-# OPTIONS_GHC -Wall #-}
module Docs.Check
  ( check
  )
  where


import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)

import qualified AST.Exposing as Exposing
import qualified Docs.AST as Docs
import qualified Elm.Compiler.Type as Type
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



-- CHECK


check :: Exposing.Canonical -> A.Located (Maybe Docs.Centralized) -> R.Result () w Error.Error Docs.Checked
check exports (A.A region maybeDocs) =
  case maybeDocs of
    Nothing ->
      R.throw region (Error.Docs E.NoDocs)

    Just docs ->
      checkHelp region exports docs


checkHelp :: R.Region -> Exposing.Canonical -> Docs.Centralized -> R.Result () w Error.Error Docs.Checked
checkHelp region exposing (Docs.Centralized comment values aliases unions) =
  do  docNames <- parseNames region comment

      let docSet = Set.fromList (map A.drop docNames)
      let restrict checker dict =
            Map.traverseWithKey checker (Map.restrictKeys dict docSet)

      R.mapError Error.Docs $
        Docs.Checked comment
          <$> restrict checkValue values
          <*> restrict checkAlias aliases
          <*> restrict (checkUnion exposing) unions
          <*  checkCommentNames region exposing docNames



-- CHECK COMMENT NAMES


type Result w a = R.Result () w E.Error a


checkCommentNames :: R.Region -> Exposing.Canonical -> [A.Located Text] -> Result w ()
checkCommentNames region (Exposing.Canonical values aliases unions) docNames =
  let
    addUnit name =
      ( name, () )

    exposed =
      Map.fromList $ map addUnit $
        values ++ aliases ++ map fst unions

    docDict =
      A.listToDict id docNames
  in
    checkOnlyInExports region (Map.difference exposed docDict)
      <* Map.traverseMaybeWithKey (checkDocName exposed) docDict


checkOnlyInExports :: R.Region -> Map.Map Text () -> Result w ()
checkOnlyInExports region onlyInExports =
  case Map.keys onlyInExports of
    [] ->
      R.ok ()

    names ->
      R.throw region (E.OnlyInExports names)


checkDocName :: Map.Map Text () -> Text -> NonEmpty R.Region -> Result w (Maybe a)
checkDocName exposed name (region :| duplicates) =
  case duplicates of
    [] ->
      if Map.member name exposed then
        R.ok Nothing

      else
        R.throw region $ E.OnlyInDocs name $
          Help.nearbyNames id name (Map.keys exposed)

    _ ->
      R.throw region (E.Duplicates name)



-- CHECK COMMENT


checkComment :: R.Region -> Text -> Maybe Text -> Result w Text
checkComment region name maybeComment =
  case maybeComment of
    Just comment ->
      R.ok comment

    Nothing ->
      R.throw region (E.NoComment name)



-- CHECK VALUE


checkValue :: Text -> A.Located Docs.RawValue -> Result w Docs.GoodValue
checkValue name (A.A region (Docs.Value comment tipe assocPrec)) =
  Docs.Value
    <$> checkComment region name comment
    <*> checkValueType region name tipe
    <*> pure assocPrec


checkValueType :: R.Region -> Text -> Maybe Type.Type -> Result w Type.Type
checkValueType region name maybeType =
  case maybeType of
    Just tipe ->
      R.ok tipe

    Nothing ->
      R.throw region (E.NoType name)



-- CHECK ALIAS


checkAlias :: Text -> A.Located Docs.RawAlias -> Result w Docs.GoodAlias
checkAlias name (A.A region (Docs.Alias comment args tipe)) =
  Docs.Alias
    <$> checkComment region name comment
    <*> pure args
    <*> pure tipe



-- CHECK UNION


checkUnion :: Exposing.Canonical -> Text -> A.Located Docs.RawUnion -> Result w Docs.GoodUnion
checkUnion (Exposing.Canonical _ _ unions) name (A.A region (Docs.Union comment args ctors)) =
  Docs.Union
    <$> checkComment region name comment
    <*> pure args
    <*> checkUnionCtors (Map.fromList unions) name ctors


checkUnionCtors :: Map.Map Text [Text] -> Text -> [(Text, [Type.Type])] -> Result w [(Text, [Type.Type])]
checkUnionCtors unions name ctors =
  let
    publicCtors =
      maybe [] id (Map.lookup name unions)

    isPublic (ctor, _) =
      ctor `elem` publicCtors
  in
    R.ok $ filter isPublic ctors



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
