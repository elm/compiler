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
checkHelp region exposing (Docs.Docs overview unions aliases values) =
  do  docNames <- parseNames region overview

      let docSet = Set.fromList (map A.drop docNames)
      let restrict checker dict =
            Map.traverseWithKey (checkEntry checker) (Map.restrictKeys dict docSet)

      R.mapError Error.Docs $
        Docs.Docs overview
          <$> restrict (checkUnion exposing) unions
          <*> restrict checkAlias aliases
          <*> restrict checkValue values
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



-- CHECK ENTRY


checkEntry :: (R.Region -> Text -> a -> Result w b) -> Text -> Docs.Raw a -> Result w (Docs.Good b)
checkEntry checkDetails name (A.A region (Docs.Entry comment details)) =
  Docs.Entry
    <$> checkComment region name comment
    <*> checkDetails region name details



-- CHECK UNION


checkUnion :: Exposing.Canonical -> R.Region -> Text -> Docs.Union -> Result w Docs.Union
checkUnion (Exposing.Canonical _ _ unions) _region name (Docs.Union args ctors) =
  let
    publicCtors =
      maybe [] id (lookup name unions)

    isPublic (ctor, _) =
      ctor `elem` publicCtors
  in
    pure $ Docs.Union args (filter isPublic ctors)



-- CHECK ALIAS


checkAlias :: R.Region -> Text -> Docs.Alias -> Result w Docs.Alias
checkAlias _ _ alias =
  pure alias



-- CHECK VALUE


checkValue :: R.Region -> Text -> Docs.RawValue -> Result w Docs.GoodValue
checkValue region name value =
  case value of
    Docs.Value tipe ->
      Docs.Value
        <$> checkValueType region name tipe

    Docs.Infix tipe assoc prec ->
      Docs.Infix
        <$> checkValueType region name tipe
        <*> pure assoc
        <*> pure prec


checkValueType :: R.Region -> Text -> Maybe Type.Type -> Result w Type.Type
checkValueType region name maybeType =
  case maybeType of
    Just tipe ->
      R.ok tipe

    Nothing ->
      R.throw region (E.NoType name)



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
