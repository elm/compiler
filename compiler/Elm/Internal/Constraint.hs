module Elm.Internal.Constraint ( Constraint(..)
                               , fromString
                               , exact
                               , satisfyConstraint
                               ) where

import Data.Aeson
import Control.Applicative ((<$>), (<*>))

import Elm.Internal.Version (Version)
import qualified Elm.Internal.Version as V

data Endpoint = Included Version
              | Excluded Version
              deriving (Show, Eq, Ord)

satisfyUpper :: Endpoint -> Version -> Bool
satisfyUpper endpoint version = case endpoint of
  Included v -> version <= v
  Excluded v -> version < v

satisfyLower :: Endpoint -> Version -> Bool
satisfyLower endpoint version = case endpoint of
  Included v -> version >= v
  Excluded v -> version > v

renderUpper :: Endpoint -> String
renderUpper endpoint = case endpoint of
  Included v -> "<=" ++ show v
  Excluded v -> "<" ++ show v

renderLower :: Endpoint -> String
renderLower endpoint = case endpoint of
  Included v -> ">=" ++ show v
  Excluded v -> ">" ++ show v

data Constraint = Range Endpoint Endpoint
                deriving (Eq)

exact :: V.Version -> Constraint
exact v = Range (Included v) (Included v)

satisfyConstraint :: Constraint -> Version -> Bool
satisfyConstraint (Range lower upper) version =
  satisfyLower lower version && satisfyUpper upper version

instance Show Constraint where
  show constr = case constr of
    Range (Included v1) (Included v2) | v1 == v2 -> "==" ++ show v1
    Range lower upper -> concat [renderLower lower, " ", renderUpper upper]

parseLower :: String -> Maybe Endpoint
parseLower str = case str of
  '>' : '=' : rest -> Included <$> V.fromString rest
  '>' : rest -> Excluded <$> V.fromString rest
  _ -> Nothing

parseUpper :: String -> Maybe Endpoint
parseUpper str = case str of
  '<' : '=' : rest -> Included <$> V.fromString rest
  '<' : rest -> Included <$> V.fromString rest
  _ -> Nothing

fromString :: String -> Maybe Constraint
fromString str = case words str of
  [elem] -> case elem of
    '=' : '=' : rest -> exact <$> V.fromString rest
    -- second case is written to ease transition from exact versions to constraints,
    -- and should be removed after a couple of releases
    _ -> exact <$> V.fromString elem
  [elem1, elem2] -> case parseLower elem1 of
    Just ep -> Range ep <$> parseUpper elem2
    Nothing -> (flip Range) <$> parseUpper elem1 <*> parseLower elem2
  _ -> Nothing

instance ToJSON Constraint where
  toJSON = toJSON . show
