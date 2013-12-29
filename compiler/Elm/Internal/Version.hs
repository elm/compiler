{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Elm.Internal.Version where

import           Control.Applicative
import           Data.Aeson
import           Data.Binary
import           Data.Char              (isDigit)
import qualified Data.List              as List
import           Data.Typeable
import qualified Data.Text              as T
import qualified Paths_Elm as This
import qualified Data.Version as Version

elmVersion :: Version
elmVersion = V ns ""
    where (Version.Version ns _) = This.version

-- Data representation

data Version = V [Int] String
    deriving (Typeable,Eq)

instance Ord Version where
  compare (V ns tag) (V ns' tag') =
      case compare ns ns' of
        EQ -> compare tag' tag -- reverse comparison to favor ""
        cmp -> cmp

instance Show Version where
  show (V ns tag) =
      List.intercalate "." (map show ns) ++ if null tag then "" else "-" ++ tag

instance Binary Version where
  get = V <$> get <*> get
  put (V ns tag) = do put ns
                      put tag

tagless :: Version -> Bool
tagless (V _ tag) = null tag

fromString :: String -> Maybe Version
fromString version = V <$> splitNumbers possibleNumbers <*> tag
    where
      (possibleNumbers, possibleTag) = break (=='-') version

      tag = case possibleTag of
              "" -> Just ""
              '-':rest -> Just rest
              _ -> Nothing

      splitNumbers :: String -> Maybe [Int]
      splitNumbers ns =
          case span isDigit ns of
            ("", _) -> Nothing
            (number, []) -> Just [read number]
            (number, '.':rest) -> (read number :) <$> splitNumbers rest
            _ -> Nothing

instance FromJSON Version where
    parseJSON (String text) =
        let string = T.unpack text in
        case fromString string of
          Just v -> return v
          Nothing -> fail $ unlines
                     [ "Dependency file has an invalid version number: " ++ string
                     , "Must have format 0.1.2 or 0.1.2-tag"
                     ]

    parseJSON _ = fail "Version number must be stored as a string."

instance ToJSON Version where
    toJSON version = toJSON (show version)

