{-# LANGUAGE OverloadedStrings #-}
module Elm.Internal.Documentation where

import Control.Applicative ((<$>),(<*>))
import Control.Monad
import Data.Aeson

data Document = Doc
    { moduleName :: String
    , structure :: String
    , entries :: [Entry]
    } deriving (Show)

data Entry = Entry
    { name :: String
    , comment :: String
    , raw :: String
    , assocPrec :: Maybe (String,Int)
    } deriving (Show)

instance FromJSON Document where
    parseJSON (Object v) =
        Doc <$> v .: "name"
            <*> v .: "document"
            <*> (concat <$> sequence [ v .: "aliases", v .: "datatypes", v .: "values" ])

    parseJSON _ = fail "Conversion to Document was expecting an object"

instance FromJSON Entry where
    parseJSON (Object v) =
        Entry <$> v .: "name"
              <*> v .: "comment"
              <*> v .: "raw"
              <*> (liftM2 (,) <$> v .:? "associativity"
                              <*> v .:? "precedence")

    parseJSON _ = fail "Conversion to Entry was expecting an object"

data Content = Markdown String | Value String
               deriving Show

