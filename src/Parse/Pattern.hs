module Parse.Pattern (term, expr) where

import qualified Data.List as List
import Text.Parsec ((<|>), (<?>), char, choice, optionMaybe, try)

import qualified AST.Literal as L
import qualified AST.Pattern as P
import qualified AST.Variable as Var
import Parse.Helpers
import qualified Parse.Literal as Literal
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


basic :: IParser P.Raw
basic =
  addLocation $
    choice
      [ char '_' >> return P.Anything
      , P.Var <$> lowVar
      , chunksToPatterns <$> dotSep1 capVar
      , P.Literal <$> Literal.literal
      ]
  where
    chunksToPatterns chunks =
      case List.intercalate "." chunks of
        "True" ->
          P.Literal (L.Boolean True)

        "False" ->
          P.Literal (L.Boolean False)

        name ->
          P.Data (Var.Raw name) []


asPattern :: IParser P.Raw -> IParser P.Raw
asPattern patternParser =
  do  pattern <- patternParser

      let (A.A (R.Region start _) _) = pattern

      maybeAlias <- optionMaybe asAlias

      case maybeAlias of
        Just alias ->
            do  end <- getMyPosition
                return (A.at start end (P.Alias alias pattern))

        Nothing ->
            return pattern
  where
    asAlias =
      do  try (whitespace >> reserved "as")
          whitespace
          lowVar


record :: IParser P.Raw
record =
  addLocation
    (P.Record <$> brackets (commaSep1 lowVar))


tuple :: IParser P.Raw
tuple =
  do  (start, patterns, end) <-
          located (parens (commaSep expr))

      case patterns of
        [pattern] ->
            return pattern

        _ ->
            return (A.at start end (P.tuple patterns))


list :: IParser P.Raw
list =
  braces $
    do  (_, patterns, end) <- located (commaSep expr)
        return (P.list end patterns)


term :: IParser P.Raw
term =
  choice [ record, tuple, list, basic ]
    <?> "a pattern"


patternConstructor :: IParser P.Raw
patternConstructor =
  addLocation $
    do  name <- List.intercalate "." <$> dotSep1 capVar
        case name of
          "True" ->
            return $ P.Literal (L.Boolean True)

          "False" ->
            return $ P.Literal (L.Boolean False)

          _ ->
            P.Data (Var.Raw name) <$> spacePrefix term


expr :: IParser P.Raw
expr =
    asPattern subPattern <?> "a pattern"
  where
    subPattern =
      do  patterns <- consSep1 (patternConstructor <|> term)
          end <- getMyPosition
          return (P.consMany end patterns)
