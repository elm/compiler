module Elm.Internal.Constraint where

import Data.Aeson
import Control.Applicative ((<$>), (<*>))

import Elm.Internal.Version (Version)
import qualified Elm.Internal.Version as V

data Endpoint = Included Version
              | Excluded Version
              deriving (Show, Eq, Ord)

satisfyUpper :: Endpoint -> Version -> Bool
satisfyUpper ep version = case ep of
  Included p -> version <= p
  Excluded p -> version < p

satisfyLower :: Endpoint -> Version -> Bool
satisfyLower ep version = case ep of
  Included p -> version >= p
  Excluded p -> version > p

renderUpper :: Endpoint -> String
renderUpper point = case point of
  Included v -> "<=" ++ show v
  Excluded v -> "<" ++ show v

renderLower :: Endpoint -> String
renderLower point = case point of
  Included v -> ">=" ++ show v
  Excluded v -> ">" ++ show v

data Constraint = Range Endpoint Endpoint
                | Exact Version
                deriving (Eq, Ord)

satisfyConstraint :: Constraint -> Version -> Bool
satisfyConstraint c version = case c of
  Range lower upper -> satisfyLower lower version && satisfyUpper upper version
  Exact v -> version == v

instance Show Constraint where
  show constr = case constr of
    Range lower upper -> concat [renderLower lower, " ", renderUpper upper]
    Exact version -> show version

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
    '=' : '=' : rest -> Exact <$> V.fromString rest
    _ -> Nothing
  [elem1, elem2] -> case parseLower elem1 of
    Just ep -> Range ep <$> parseUpper elem2
    Nothing -> (flip Range) <$> parseUpper elem1 <*> parseLower elem2
  _ -> Nothing

instance ToJSON Constraint where
  toJSON = toJSON . show
