{-# LANGUAGE OverloadedStrings #-}
module Elm.Internal.Dependencies where

import Control.Applicative
import Control.Monad.Error
import qualified Control.Exception as E
import Data.Aeson
import qualified Data.List as List
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as Map
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Version as V
import qualified Elm.Internal.Paths as Path

data Deps = Deps
    { name :: N.Name
    , version :: V.Version
    , summary :: String
    , description :: String
    , license :: String
    , repo :: String
    , exposed :: [String]
    , elmVersion :: V.Version
    , dependencies :: [(N.Name,V.Version)]
    } deriving Show

data MiniDeps = Mini [(N.Name,V.Version)]

instance FromJSON MiniDeps where
    parseJSON (Object obj) = Mini <$> getDependencies obj
    parseJSON _ = mzero

instance FromJSON Deps where
    parseJSON (Object obj) =
        do version <- get obj "version" "your projects version number"

           summary <- get obj "summary" "a short summary of your project"
           when (length summary >= 80) $
               fail "'summary' must be less than 80 characters"

           desc <- get obj "description" "an extended description of your project \
                                         \and how to get started with it."
           license <- get obj "license" "license information (BSD3 is recommended)"

           repo <- get obj "repository" "a link to the project's GitHub repo"
           name <- case repoToName repo of
                     Left err -> fail err
                     Right nm -> return nm

           exposed <- get obj "exposed-modules" "a list of modules exposed to users"

           elmVersion <- get obj "elm-version" "the version of the Elm compiler you are using"

           deps <- getDependencies obj

           return $ Deps name version summary desc license repo exposed elmVersion deps

    parseJSON _ = mzero

getDependencies obj = 
    toDeps =<< get obj "dependencies" "a listing of your project's dependencies"
    where
      toDeps deps =
          forM (Map.toList deps) $ \(f,v) ->
              case (N.fromString f, V.fromString v) of
                (Just name, Just version) -> return (name,version)
                (Nothing, _) -> fail $ N.errorMsg f
                (_, Nothing) -> fail $ "invalid version number " ++ v


get obj field desc =
    do maybe <- obj .:? field
       case maybe of
         Just value -> return value
         Nothing -> fail $ "Missing field " ++ show field ++ ", " ++ desc ++ ".\n" ++
                           "    Check out an example " ++ Path.dependencyFile ++ " file here:" ++
                           "    <https://github.com/evancz/automaton/blob/master/elm_dependencies.json>"

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

depsAt :: FilePath -> ErrorT String IO Deps
depsAt = withDeps id