{-# LANGUAGE OverloadedStrings #-}
module Elm.Internal.Dependencies where

import Control.Applicative
import Control.Monad.Error
import qualified Control.Exception as E
import Data.Aeson
import qualified Data.List as List
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Version as V

data Deps = Deps
    { name :: N.Name
    , version :: V.Version
    , summary :: String
    , description :: String
    , license :: String
    , repo :: String
    , exposed :: [String]
    , elmVersion :: V.Version
    } deriving Show

instance FromJSON Deps where
    parseJSON (Object obj) =
        do version <- obj .: "version"

           summary <- obj .: "summary"
           when (length summary >= 80) $
               fail "'summary' must be less than 80 characters"

           desc    <- obj .: "description"
           license <- obj .: "license"

           repo <- obj .: "repository"
           name <- case repoToName repo of
                     Left err -> fail err
                     Right nm -> return nm

           exposed <- obj .: "exposed-modules"
           when (null exposed) $
                fail "there are no 'exposed-modules'.\n\
                     \At least one module must be exposed for anyone to use this library!"

           elmVersion <- obj .: "elm-version"

           return $ Deps name version summary desc license repo exposed elmVersion

    parseJSON _ = mzero

repoToName :: String -> Either String N.Name
repoToName repo
    | not (end `List.isSuffixOf` repo) = Left msg
    | otherwise =
        do path <- getPath
           let raw = take (length path - length end) path
           case N.fromString raw of
             Nothing   -> Left msg
             Just name -> Right name
    where
      getPath | http  `List.isPrefixOf` repo = Right $ drop (length http ) repo
              | https `List.isPrefixOf` repo = Right $ drop (length https) repo
              | otherwise = Left msg
      http  = "http://github.com/"
      https = "https://github.com/"
      end = ".git"
      msg = "the 'repository' field must point to a GitHub project for now, something\n\
            \like <https://github.com/USER/PROJECT.git> where USER is your GitHub name\n\
            \and PROJECT is the repo you want to upload."

withDeps :: (Deps -> a) -> FilePath -> ErrorT String IO a
withDeps handle path =
    do json <- readPath
       case eitherDecode json of
         Left err -> throwError $ "Error reading file " ++ path ++ ":\n    " ++ err
         Right ds -> return (handle ds)
    where
      readPath :: ErrorT String IO BS.ByteString
      readPath = do
        result <- liftIO $ E.catch (Right <$> BS.readFile path)
                                   (\err -> return $ Left (err :: IOError))
        case result of
          Right bytes -> return bytes
          Left _ -> throwError $
                    "could not find file " ++ path ++
                    "\n    You may need to create a dependency file for your project."

dependencies :: FilePath -> ErrorT String IO (Map.Map String String)
dependencies _ = return Map.empty -- withDeps deps

depsAt :: FilePath -> ErrorT String IO Deps
depsAt = withDeps id