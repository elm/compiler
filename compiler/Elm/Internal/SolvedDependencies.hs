{-# OPTIONS_GHC -Wall #-}
module Elm.Internal.SolvedDependencies where

import Prelude hiding (read)
import Control.Monad (when)
import Control.Monad.Error (runErrorT, throwError, ErrorT, liftIO)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text
import qualified Data.Map as Map
import System.Directory (doesFileExist)

import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Version as V


-- CONVERSION TO JSON

toJson :: [(N.Name, V.Version)] -> Value
toJson deps =
    object (map toField deps)
  where
    toField (name, version) =
        Text.pack (N.toString name) .= Text.pack (V.toString version)

fromJson :: Map.Map String String -> ErrorT String IO [(N.Name, V.Version)]
fromJson pairs =
    mapM convert (Map.toList pairs)
  where
    convert :: (String, String) -> ErrorT String IO (N.Name, V.Version)
    convert (name, version) =
        case N.fromString name of
          Nothing ->
              throwError $ "Could not parse package name " ++ name

          Just realName ->
              case V.fromString version of
                Nothing ->
                    throwError $ "Could not parse version number for package " ++ name

                Just realVersion ->
                    return (realName, realVersion)


-- READING AND WRITING FILES

write :: FilePath -> [(N.Name, V.Version)] -> IO ()
write filePath pairs =
    BS.writeFile filePath (encodePretty (toJson pairs))

readAnd :: FilePath -> ([(N.Name, V.Version)] -> ErrorT String IO a) -> ErrorT String IO a
readAnd path handle =
  do exists <- liftIO (doesFileExist path)
     when (not exists) throwDoesNotExist
     byteString <- liftIO $ BS.readFile path
     pairs <- either throwCorrupted fromJson (eitherDecode byteString)
     handle pairs
  where
    throwCorrupted _msg =
        throwError $ "Unable to extract package information from the " ++ path ++
                     " file.\nIt may be corrupted."

    throwDoesNotExist =
        throwError ("File " ++ path ++ " does not exist.")


read :: FilePath -> ErrorT String IO [(N.Name, V.Version)]
read path = readAnd path return

readMaybe :: FilePath -> IO (Maybe [(N.Name, V.Version)])
readMaybe path =
  do eitherPairs <- runErrorT (read path)
     case eitherPairs of
       Right pairs -> return (Just pairs)
       Left _ -> return Nothing
