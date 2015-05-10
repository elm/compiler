module Parse.Pattern (term, expr) where

import Control.Applicative ((<$>))
import Data.Char (isUpper)
import qualified Data.List as List
import Text.Parsec ((<|>), (<?>), char, choice, optionMaybe, try)

import qualified AST.Literal as L
import qualified AST.Pattern as P
import qualified AST.Variable as Var
import Parse.Helpers
import qualified Parse.Literal as Literal
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


basic :: IParser P.RawPattern
basic =
  addLocation $
    choice
      [ char '_' >> return P.Anything
      , stringToPattern <$> var
      , P.Literal <$> Literal.literal
      ]
  where
    stringToPattern str =
        case str of
          "True" ->
              P.Literal (L.Boolean True)

          "False" ->
              P.Literal (L.Boolean False)

          c:_ | isUpper c ->
              P.Data (Var.Raw str) []

          _ ->
              P.Var str


asPattern :: IParser P.RawPattern -> IParser P.RawPattern
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


record :: IParser P.RawPattern
record =
  addLocation
    (P.Record <$> brackets (commaSep1 lowVar))


tuple :: IParser P.RawPattern
tuple =
  do  (start, patterns, end) <-
          located (parens (commaSep expr))

      case patterns of
        [pattern] ->
            return pattern

        _ ->
            return (A.at start end (P.tuple patterns))


list :: IParser P.RawPattern
list =
  braces $
    do  (_, patterns, end) <- located (commaSep expr)
        return (P.list end patterns)


term :: IParser P.RawPattern
term =
  choice [ record, tuple, list, basic ]
    <?> "a pattern"


patternConstructor :: IParser P.RawPattern
patternConstructor =
  addLocation $
    do  v <- List.intercalate "." <$> dotSep1 capVar
        case v of
          "True"  -> return $ P.Literal (L.Boolean True)
          "False" -> return $ P.Literal (L.Boolean False)
          _       -> P.Data (Var.Raw v) <$> spacePrefix term


expr :: IParser P.RawPattern
expr =
    asPattern subPattern <?> "a pattern"
  where
    subPattern =
      do  patterns <- consSep1 (patternConstructor <|> term)
          end <- getMyPosition
          return (P.consMany end patterns)
