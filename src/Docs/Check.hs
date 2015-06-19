module Docs.Check (check) where

import Control.Applicative ((<$>),(<*>))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Traversable as T
import Text.Parsec ((<|>), choice, many, string)

import qualified AST.Variable as Var
import qualified Docs.AST as Docs
import qualified Elm.Compiler.Type as Type
import Elm.Utils ((|>))
import Parse.Helpers
    ( anyUntil, commaSep1, iParse, parens, simpleNewline, symOp, var
    , whitespace
    )
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Docs as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as R


type Result w a = R.Result w Error.Error a


-- CHECK DOCUMENTATION

check :: [Var.Value] -> A.Located (Maybe Docs.Centralized) -> Result w Docs.Checked
check exports (A.A region maybeDocs) =
  case maybeDocs of
    Nothing ->
        R.throw region Error.NoDocs

    Just docs ->
        checkHelp region exports docs


checkHelp :: R.Region -> [Var.Value] -> Docs.Centralized -> Result w Docs.Checked
checkHelp region exports (Docs.Docs comment aliases types values) =
  let
    docNames =
      Set.fromList (commentedNames comment)

    checkCategory checkValue dict =
      dict
        |> Map.filterWithKey (\key _ -> Set.member key docNames)
        |> Map.mapWithKey checkValue
        |> T.traverse id

    exportedUnions =
      map (\(tipe, Var.Listing tags _) -> (tipe, tags)) (Var.getUnions exports)
  in
    (\() -> Docs.Docs comment)
      <$> checkModuleComment region exports docNames
      <*> checkCategory (checkComment Docs.aliasComment) aliases
      <*> checkCategory (checkUnion exportedUnions) types
      <*> checkCategory checkValue values


checkComment :: (a -> Maybe String) -> String -> A.Located a -> Result w (A.Located a)
checkComment getComment name (A.A region value) =
  case getComment value of
    Nothing ->
        R.throw region (Error.NoComment name)

    Just _ ->
        R.ok (A.A region value)


checkUnion :: [(String, [String])] -> String -> A.Located Docs.Union -> Result w (A.Located Docs.Union)
checkUnion exportedUnions name value =
  const
    <$> filterUnionTags exportedUnions name value
    <*> checkComment Docs.unionComment name value


filterUnionTags :: [(String, [String])] -> String -> A.Located Docs.Union -> Result w (A.Located Docs.Union)
filterUnionTags exportedUnions name (A.A region union@(Docs.Union _ _ ctors)) =
  case List.lookup name exportedUnions of
    Nothing ->
      R.throw region (Error.OnlyInExports name)

    Just tags ->
      R.ok $ A.A region $ union
        { Docs.unionCases =
            filter (\(tag, _) -> elem tag tags) ctors
        }



checkValue :: String -> A.Located (Docs.Value (Maybe Type.Type)) -> Result w (A.Located (Docs.Value Type.Type))
checkValue name value =
  const
    <$> hasType name value
    <*> checkComment Docs.valueComment name value


hasType :: String -> A.Located (Docs.Value (Maybe Type.Type)) -> Result w (A.Located (Docs.Value Type.Type))
hasType name (A.A region value) =
  case Docs.valueType value of
    Just tipe ->
        R.ok (A.A region (value { Docs.valueType = tipe }))

    Nothing ->
        R.throw region (Error.NoType name)


-- CHECK MODULE COMMENT

checkModuleComment :: R.Region -> [Var.Value] -> Set.Set String -> Result w ()
checkModuleComment region exports docNames =
  let
    exportNames =
      Set.fromList (map valueName exports)

    onlyInDocs =
      Set.toList (Set.difference docNames exportNames)

    onlyInExports =
      Set.toList (Set.difference exportNames docNames)
  in
    (\_ _ -> ())
      <$> T.traverse (R.throw region . Error.OnlyInDocs) onlyInDocs
      <*> T.traverse (R.throw region . Error.OnlyInExports) onlyInExports


commentedNames :: String -> [String]
commentedNames comment =
  let
    nameParser =
      choice
        [ do  string "@docs"
              whitespace
              commaSep1 (var <|> parens symOp)
        , do  anyUntil simpleNewline
              return []
        ]
  in
    case iParse (concat <$> many nameParser) comment of
      Left _ ->
          []

      Right names ->
          names


valueName :: Var.Value -> String
valueName value =
  case value of
    Var.Value name -> name
    Var.Alias name -> name
    Var.Union name _ -> name
