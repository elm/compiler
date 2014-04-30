{-# LANGUAGE DeriveDataTypeable #-}
module Elm.Internal.Name where

import Control.Applicative
import Control.Monad.Error
import Data.Aeson
import Data.Binary
import qualified Data.Text as T
import qualified Data.Maybe as Maybe
import Data.Typeable

data Name = Name { user :: String, project :: String }
    deriving (Typeable,Eq, Ord)

instance Binary Name where
  get = Name <$> get <*> get
  put (Name user project) =
      put user >> put project

instance Show Name where
  show name = user name ++ "/" ++ project name

toFilePath :: Name -> FilePath
toFilePath name = user name ++ "-" ++ project name

fromString :: String -> Maybe Name
fromString string =
    case break (=='/') string of
      ( user@(_:_), '/' : project@(_:_) )
          | all (/='/') project -> Just (Name user project)
      _ -> Nothing

fromString' :: String -> ErrorT String IO Name
fromString' string =
    Maybe.maybe (throwError $ errorMsg string) return (fromString string)

instance FromJSON Name where
    parseJSON (String text) =
        let string = T.unpack text in
        Maybe.maybe (fail $ errorMsg string) return (fromString string)

    parseJSON _ = fail "Project name must be a string."

instance ToJSON Name where
    toJSON name = toJSON (show name)

errorMsg string =
    unlines
    [ "Dependency file has an invalid name: " ++ string
    , "Must have format user/project and match a public github project."
    ]
