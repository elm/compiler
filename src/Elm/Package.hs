{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Package
  ( Name(..)
  , Package(..)
  , dummyName, kernel, core
  , browser, virtualDom, html
  , webgl, linearAlgebra
  , toString, toUrl, toFilePath
  , fromText
  , Version(..)
  , initialVersion, dummyVersion
  , bumpPatch, bumpMinor, bumpMajor
  , filterLatest, majorAndMinor
  , versionToString, versionToText, versionFromText
  , decoder
  , encode
  , versionDecoder
  , encodeVersion
  )
  where


import Control.Monad (liftM, liftM2, liftM3)
import Data.Binary (Binary, get, getWord8, put, putWord8)
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
import Data.Word (Word16)
import System.FilePath ((</>))
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode



-- PACKGE NAMES


data Name =
  Name
    { _user :: !Text
    , _project :: !Text
    }
    deriving (Eq, Ord)


data Package =
  Package
    { _name :: !Name
    , _version :: !Version
    }
    deriving (Eq, Ord)


{-# NOINLINE dummyName #-}
dummyName :: Name
dummyName =
  Name "user" "project"


{-# NOINLINE kernel #-}
kernel :: Name
kernel =
  Name "elm-lang" "kernel"


{-# NOINLINE core #-}
core :: Name
core =
  Name "elm-lang" "core"


{-# NOINLINE browser #-}
browser :: Name
browser =
  Name "elm-lang" "browser"


{-# NOINLINE virtualDom #-}
virtualDom :: Name
virtualDom =
  Name "elm-lang" "virtual-dom"


{-# NOINLINE html #-}
html :: Name
html =
  Name "elm-lang" "html"


{-# NOINLINE webgl #-}
webgl :: Name
webgl =
  Name "elm-community" "webgl"


{-# NOINLINE linearAlgebra #-}
linearAlgebra :: Name
linearAlgebra =
  Name "elm-community" "linear-algebra"


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

  else if Text.isInfixOf "." text then
    Left "Dots are not allowed in package names."

  else if Text.any Char.isUpper text then
    Left "Upper case characters are not allowed in package names."

  else if not (Char.isLetter (Text.head text)) then
    Left "Package names must start with a letter."

  else if Text.isSuffixOf "-" text then
    Left "Package names cannot end with a dash."

  else
    Right text



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



-- BINARY


instance Binary Name where
  get =
    liftM2 Name get get

  put (Name user project) =
    do  put user
        put project


instance Binary Package where
  get =
    liftM2 Package get get

  put (Package name version) =
    do  put name
        put version


instance Binary Version where
  get =
    do  word <- getWord8
        if word == 0
          then liftM3 Version get get get
          else
            do  minor <- liftM fromIntegral getWord8
                patch <- liftM fromIntegral getWord8
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



-- JSON


decoder :: Decode.Decoder Name
decoder =
  do  txt <- Decode.text
      case fromText txt of
        Left msg ->
          Decode.fail $ "Expecting a PACKAGE name. " ++ msg

        Right name ->
          Decode.succeed name


encode :: Name -> Encode.Value
encode name =
  Encode.text (toText name)



versionDecoder :: Decode.Decoder Version
versionDecoder =
  do  txt <- Decode.text
      case versionFromText txt of
        Right version ->
          Decode.succeed version

        Left msg ->
          Decode.fail $ "Expecting a VERSION. " ++ msg


encodeVersion :: Version -> Encode.Value
encodeVersion version =
  Encode.text (versionToText version)
