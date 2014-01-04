{-# OPTIONS_GHC -W #-}
module Parse.Pattern (term, expr) where

import Control.Applicative ((<$>))
import Data.Char (isUpper)
import Data.List (intercalate)
import Text.Parsec hiding (newline,spaces,State)

import Parse.Helpers
import Parse.Literal
import SourceSyntax.Literal
import SourceSyntax.Pattern hiding (tuple, list)
import qualified SourceSyntax.Pattern as Pattern

basic :: IParser Pattern
basic = choice
    [ char '_' >> return PAnything
    , do v <- var
         return $ case v of
                    "True"          -> PLiteral (Boolean True)
                    "False"         -> PLiteral (Boolean False)
                    c:_ | isUpper c -> PData v []
                    _               -> PVar v
    , PLiteral <$> literal
    ]

asPattern :: Pattern -> IParser Pattern
asPattern pattern = do
  var <- optionMaybe (try (whitespace >> reserved "as" >> whitespace >> lowVar))
  return $ case var of
             Just v -> PAlias v pattern
             Nothing -> pattern

record :: IParser Pattern
record = PRecord <$> brackets (commaSep1 lowVar)

tuple :: IParser Pattern
tuple = do ps <- parens (commaSep expr)
           return $ case ps of { [p] -> p; _ -> Pattern.tuple ps }

list :: IParser Pattern
list = Pattern.list <$> braces (commaSep expr)

term :: IParser Pattern
term =
     (choice [ record, tuple, list, basic ]) <?> "pattern"

patternConstructor :: IParser Pattern
patternConstructor = do
  v <- intercalate "." <$> dotSep1 capVar
  case v of
    "True"  -> return $ PLiteral (Boolean True)
    "False" -> return $ PLiteral (Boolean False)
    _       -> PData v <$> spacePrefix term

expr :: IParser Pattern
expr = do
  patterns <- consSep1 (patternConstructor <|> term)
  asPattern (foldr1 Pattern.cons patterns) <?> "pattern"
