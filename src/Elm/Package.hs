{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Package
  ( Name(..)
  , Package(..)
  , dummyName, core, virtualDom, html
  , toString, toText, toUrl, toFilePath
  , fromText
  , Version(..)
  , initialVersion, dummyVersion
  , bumpPatch, bumpMinor, bumpMajor
  , filterLatest, majorAndMinor
  , versionToString, versionToText, versionFromText
  )
  where

import qualified Data.Aeson as Json
import Data.Binary
import qualified Data.Char as Char
import Data.Function (on)
import qualified Data.List as List
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import qualified Data.Text.Lazy as B (toStrict)
import qualified Data.Text.Lazy.Builder as B (singleton, toLazyText)
import qualified Data.Text.Lazy.Builder.Int as B (decimal)
import Data.Text (Text)
import System.FilePath ((</>))



-- PACKGE NAMES


data Name =
  Name
    { _user :: !Text
    , _project :: !Text
    }
    deriving (Eq, Ord, Show)


data Package =
  Package
    { _name :: !Name
    , _version :: !Version
    }


dummyName :: Name
dummyName =
    Name "user" "project"


core :: Name
core =
  Name "elm-lang" "core"


virtualDom :: Name
virtualDom =
  Name "elm-lang" "virtual-dom"


html :: Name
html =
  Name "elm-lang" "html"


toString :: Name -> String
toString name =
    Text.unpack (toText name)


toText :: Name -> Text
toText (Name user project) =
    user <> "/" <> project


toUrl :: Name -> String
toUrl (Name user project) =
    Text.unpack user ++ "/" ++ Text.unpack project


toFilePath :: Name -> FilePath
toFilePath (Name user project) =
    Text.unpack user </> Text.unpack project


fromText :: Text -> Either String Name
fromText text =
  case Text.splitOn "/" text of
    [ user, project ] | not (Text.null user || Text.null project) ->
      Name user <$> validateProjectName project

    _ ->
      Left "A valid project name looks like `user/project`"


validateProjectName :: Text -> Either String Text
validateProjectName text =
  if Text.isInfixOf "--" text then
    Left "There is a double dash -- in your package name. It must be a single dash."

  else if Text.isInfixOf "_" text then
    Left "Underscores are not allowed in package names."

  else if Text.any Char.isUpper text then
    Left "Upper case characters are not allowed in package names."

  else if not (Char.isLetter (Text.head text)) then
    Left "Package names must start with a letter."

  else
    Right text


instance Binary Name where
  get =
    Name <$> get <*> get

  put (Name user project) =
    do  put user
        put project


instance Json.FromJSON Name where
  parseJSON (Json.String text) =
    case fromText text of
      Left msg ->
        fail $
          "Ran into an invalid package name: "
          ++ Text.unpack text ++ "\n\n" ++ msg

      Right name ->
        return name

  parseJSON _ =
    fail "Project name must be a string."


instance Json.ToJSON Name where
  toJSON name =
    Json.String (toText name)



-- PACKAGE VERSIONS


data Version =
  Version
    { _major :: {-# UNPACK #-} !Word16
    , _minor :: {-# UNPACK #-} !Word16
    , _patch :: {-# UNPACK #-} !Word16
    }
    deriving (Eq, Ord)


initialVersion :: Version
initialVersion =
    Version 1 0 0

dummyVersion :: Version
dummyVersion =
    Version 0 0 0


bumpPatch :: Version -> Version
bumpPatch (Version major minor patch) =
    Version major minor (patch + 1)


bumpMinor :: Version -> Version
bumpMinor (Version major minor _patch) =
    Version major (minor + 1) 0


bumpMajor :: Version -> Version
bumpMajor (Version major _minor _patch) =
    Version (major + 1) 0 0



-- FILTERING


filterLatest :: (Ord a) => (Version -> a) -> [Version] -> [Version]
filterLatest characteristic versions =
    map last (List.groupBy ((==) `on` characteristic) (List.sort versions))


majorAndMinor :: Version -> ( Int, Int )
majorAndMinor (Version major minor _patch) =
    ( fromIntegral major, fromIntegral minor )



-- CONVERSIONS


versionToString :: Version -> String
versionToString version =
  Text.unpack (versionToText version)


versionToText :: Version -> Text
versionToText (Version major minor patch) =
  B.toStrict $ B.toLazyText $
    B.decimal major
    <> B.singleton '.'
    <> B.decimal minor
    <> B.singleton '.'
    <> B.decimal patch


versionFromText :: Text -> Either String Version
versionFromText text =
  case Text.splitOn "." text of
    [major, minor, patch] ->
      Version
        <$> toNumber major
        <*> toNumber minor
        <*> toNumber patch

    _ ->
      Left "Must have format MAJOR.MINOR.PATCH (e.g. 1.0.2)"


toNumber :: Text -> Either String Word16
toNumber txt =
  case Text.decimal txt of
    Right (n, "") ->
      Right n

    _ ->
      Left "Must have format MAJOR.MINOR.PATCH (e.g. 1.0.2)"


instance Binary Version where
  get =
    do  word <- getWord8
        if word == 0
          then Version <$> get <*> get <*> get
          else
            do  minor <- fromIntegral <$> getWord8
                patch <- fromIntegral <$> getWord8
                return (Version (fromIntegral word) minor patch)

  put (Version major minor patch) =
    if major < 256 && minor < 256 && patch < 256 then
      do  putWord8 (fromIntegral major)
          putWord8 (fromIntegral minor)
          putWord8 (fromIntegral patch)
    else
      do  putWord8 0
          put major
          put minor
          put patch


instance Json.FromJSON Version where
  parseJSON (Json.String text) =
    case versionFromText text of
      Right version ->
        return version

      Left problem ->
        fail $ unlines $
          [ "Ran into an invalid version number: " ++ Text.unpack text
          , problem
          ]

  parseJSON _ =
    fail "Version number must be stored as a string."


instance Json.ToJSON Version where
  toJSON version =
    Json.String (versionToText version)

