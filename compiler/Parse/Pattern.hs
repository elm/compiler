{-# OPTIONS_GHC -W #-}
module Parse.Pattern (term, expr) where

import Control.Applicative ((<$>))
import Data.Char (isUpper)
import qualified Data.List as List
import Text.Parsec hiding (newline,spaces,State)

import Parse.Helpers
import Parse.Literal
import SourceSyntax.Literal
import qualified SourceSyntax.Pattern as P

basic :: IParser P.Pattern
basic = choice
    [ char '_' >> return P.Anything
    , do v <- var
         return $ case v of
                    "True"          -> P.Literal (Boolean True)
                    "False"         -> P.Literal (Boolean False)
                    c:_ | isUpper c -> P.Data v []
                    _               -> P.Var v
    , P.Literal <$> literal
    ]

asPattern :: P.Pattern -> IParser P.Pattern
asPattern pattern = do
  var <- optionMaybe (try (whitespace >> reserved "as" >> whitespace >> lowVar))
  return $ case var of
             Just v -> P.Alias v pattern
             Nothing -> pattern

record :: IParser P.Pattern
record = P.Record <$> brackets (commaSep1 lowVar)

tuple :: IParser P.Pattern
tuple = do
  ps <- parens (commaSep expr)
  return $ case ps of
             [p] -> p
             _ -> P.tuple ps

list :: IParser P.Pattern
list = P.list <$> braces (commaSep expr)

term :: IParser P.Pattern
term =
     (choice [ record, tuple, list, basic ]) <?> "pattern"

patternConstructor :: IParser P.Pattern
patternConstructor = do
  v <- List.intercalate "." <$> dotSep1 capVar
  case v of
    "True"  -> return $ P.Literal (Boolean True)
    "False" -> return $ P.Literal (Boolean False)
    _       -> P.Data v <$> spacePrefix term

expr :: IParser P.Pattern
expr = do
  patterns <- consSep1 (patternConstructor <|> term)
  asPattern (foldr1 P.cons patterns) <?> "pattern"
