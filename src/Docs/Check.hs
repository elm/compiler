module Docs.Check (check) where

import Control.Applicative ((<$>),(<*>))
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


-- CHECK DOCUMENTATION

check :: [Var.Value] -> A.Located (Maybe Docs.Centralized) -> R.Result w Error.Error Docs.Checked
check exports (A.A region maybeDocs) =
  case maybeDocs of
    Nothing ->
        R.throw region Error.NoDocs

    Just docs ->
        checkHelp region exports docs


checkHelp :: R.Region -> [Var.Value] -> Docs.Centralized -> R.Result w Error.Error Docs.Checked
checkHelp region exports (Docs.Docs comment aliases types values) =
  let
    docNames =
      Set.fromList (commentedNames comment)

    checkCategory checkValue dict =
      dict
        |> Map.filterWithKey (\key _ -> Set.member key docNames)
        |> Map.mapWithKey checkValue
        |> T.traverse id

    checkValue name value =
      const
        <$> hasType name value
        <*> hasComment Docs.valueComment name value
  in
    (\() -> Docs.Docs comment)
      <$> checkComment region exports docNames
      <*> checkCategory (hasComment Docs.aliasComment) aliases
      <*> checkCategory (hasComment Docs.unionComment) types
      <*> checkCategory checkValue values


hasComment
    :: (a -> Maybe String)
    -> String
    -> A.Located a
    -> R.Result w Error.Error (A.Located a)
hasComment getComment name (A.A region value) =
  case getComment value of
    Nothing ->
        R.throw region (Error.NoComment name)

    Just _ ->
        R.ok (A.A region value)


hasType
    :: String
    -> A.Located (Docs.Value (Maybe Type.Type))
    -> R.Result w Error.Error (A.Located (Docs.Value Type.Type))
hasType name (A.A region value) =
  case Docs.valueType value of
    Just tipe ->
        R.ok (A.A region (value { Docs.valueType = tipe }))

    Nothing ->
        R.throw region (Error.NoType name)


-- CHECK MODULE COMMENT

checkComment :: R.Region -> [Var.Value] -> Set.Set String -> R.Result w Error.Error ()
checkComment region exports docNames =
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
