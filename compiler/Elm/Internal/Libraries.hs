{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Elm.Internal.Libraries where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Error (throwError, MonadError)
import Data.Aeson
import Data.Char (toUpper)
import GHC.Generics (Generic)
import qualified Control.Exception as E
import qualified Data.ByteString.Lazy as BS

import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Version as V

newtype Libraries = Libraries { getLibraries :: [Library] }
                    deriving Show

data Library = Library { libraryName :: N.Name
                       , libraryVersion :: V.Version
                       } deriving (Generic, Show)

instance FromJSON Library
instance ToJSON Library

instance FromJSON Libraries where
  parseJSON (Object a) = Libraries <$> a .: "libraries"
  parseJSON _ = fail "Tried to parse Libraries structure from non-object"

instance ToJSON Libraries where
  toJSON (Libraries libs) =
    object [ "_comment" .= map toUpper msg
           , "libraries" .= libs ]
    where msg = "This file is automatically generated. Don't change it!"

libToTuple :: Library -> (N.Name, V.Version)
libToTuple lib = (libraryName lib, libraryVersion lib)

withVersions :: (MonadError String m, MonadIO m) => FilePath -> ([(N.Name, V.Version)] -> m a) -> m a
withVersions path handle =
  do let catch err = return $ Left (err :: IOError)
     fileRead <- liftIO $ E.catch (Right <$> BS.readFile path) catch
     case fileRead of
       Right bytes -> case eitherDecode bytes of
         Right (Libraries libs) -> handle $ map libToTuple libs
         Left parseErr -> throwError $ "Can't parse exact versions from " ++ path
       Left err ->
         do liftIO $ putStrLn $ "Error reading file " ++ path
            handle []

getVersions :: (MonadError String m, MonadIO m) => FilePath -> m [(N.Name, V.Version)]
getVersions path = withVersions path return

getVersionsSafe :: MonadIO m => FilePath -> m (Maybe [(N.Name, V.Version)])
getVersionsSafe path =
  do let catch err = return $ Left (err :: IOError)
     fileRead <- liftIO $ E.catch (Right <$> BS.readFile path) catch
     case fileRead of
       Right bytes -> return $ fmap (map libToTuple . getLibraries) $ decode bytes
       Left _ -> return Nothing
