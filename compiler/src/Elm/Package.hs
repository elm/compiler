{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Package
  ( Name(..)
  , Package(..)
  , isKernel
  , toString, toText, toUrl, toFilePath
  , fromText
  , dummyName, kernel, core
  , browser, virtualDom, html
  , json, http, url
  , webgl, linearAlgebra
  , suggestions
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
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import qualified Data.Text.Lazy as B (toStrict)
import qualified Data.Text.Lazy.Builder as B (singleton, toLazyText)
import qualified Data.Text.Lazy.Builder.Int as B (decimal)
import Data.Text (Text)
import Data.Word (Word16)
import System.FilePath ((</>))

import qualified Elm.Name as N
import qualified Json.Decode.Internals as Decode
import qualified Json.Encode as Encode



-- PACKGE NAMES


data Name =
  Name
    { _author :: !Text
    , _project :: !Text
    }
    deriving (Eq, Ord)


data Package =
  Package
    { _name :: !Name
    , _version :: !Version
    }
    deriving (Eq, Ord)



-- HELPERS


isKernel :: Name -> Bool
isKernel (Name author _) =
  author == "elm" || author == "elm-explorations"


toString :: Name -> String
toString name =
    Text.unpack (toText name)


toText :: Name -> Text
toText (Name author project) =
    author <> "/" <> project


toUrl :: Name -> String
toUrl (Name author project) =
    Text.unpack author ++ "/" ++ Text.unpack project


toFilePath :: Name -> FilePath
toFilePath (Name author project) =
    Text.unpack author </> Text.unpack project


fromText :: Text -> Either (String, [String]) Name
fromText text =
  case Text.splitOn "/" text of
    [ author, project ] | not (Text.null author || Text.null project) ->
      Name author <$> validateProjectName project

    _ ->
      Left
        ( "A valid package name looks like \"author/project\" requiring the author and project name together."
        , []
        )


validateProjectName :: Text -> Either (String, [String]) Text
validateProjectName text =
  if Text.isInfixOf "--" text then
    Left $ thisCannot "have two dashes in a row" text (Text.replace "--" "-" text)

  else if Text.isInfixOf "_" text then
    Left $ thisCannot "have underscores" text (Text.replace "_" "-" text)

  else if Text.isInfixOf "." text then
    Left $ thisCannot "have dots" text (Text.replace "." "-" text)

  else if Text.any Char.isUpper text then
    Left $ thisCannot "have upper case letters" text (decap text)

  else if not (Char.isLetter (Text.head text)) then
    Left
      ( "The project name \"" ++ Text.unpack text ++ "\" must start with a letter. Is there another name that captures what your package is for?"
      , []
      )

  else if Text.isSuffixOf "-" text then
    Left $ thisCannot "end with a dash" text (Text.dropEnd 1 text)

  else
    Right text


thisCannot :: String -> Text -> Text -> (String, [String])
thisCannot beLikeThis problemName suggestedName =
  let name = Text.unpack suggestedName in
  (
    "The project name \"" ++ Text.unpack problemName ++ "\" cannot "
    ++ beLikeThis ++ ". Try something like \"" ++ name ++ "\" instead?"
  ,
    [name]
  )


decap :: Text -> Text
decap text =
  let
    chunks =
      uncurry (:) (Text.foldr gatherCaps ([],[]) text)
  in
  Text.pack $
    if any (\c -> length c == 1) chunks then
      concat chunks
    else
      List.intercalate "-" chunks


gatherCaps :: Char -> (String, [String]) -> (String, [String])
gatherCaps char (buffer, chunks) =
  if Char.isUpper char then
    ( [], (Char.toLower char : buffer) : chunks )
  else
    (char:buffer, chunks)



-- COMMON PACKAGE NAMES


{-# NOINLINE dummyName #-}
dummyName :: Name
dummyName =
  Name "author" "project"


{-# NOINLINE kernel #-}
kernel :: Name
kernel =
  Name "elm" "kernel"


{-# NOINLINE core #-}
core :: Name
core =
  Name "elm" "core"


{-# NOINLINE browser #-}
browser :: Name
browser =
  Name "elm" "browser"


{-# NOINLINE virtualDom #-}
virtualDom :: Name
virtualDom =
  Name "elm" "virtual-dom"


{-# NOINLINE html #-}
html :: Name
html =
  Name "elm" "html"


{-# NOINLINE json #-}
json :: Name
json =
  Name "elm" "json"


{-# NOINLINE http #-}
http :: Name
http =
  Name "elm" "http"


{-# NOINLINE url #-}
url :: Name
url =
  Name "elm" "url"


{-# NOINLINE webgl #-}
webgl :: Name
webgl =
  Name "elm-explorations" "webgl"


{-# NOINLINE linearAlgebra #-}
linearAlgebra :: Name
linearAlgebra =
  Name "elm-explorations" "linear-algebra"



-- PACKAGE SUGGESTIONS


suggestions :: Map.Map N.Name Name
suggestions =
  Map.fromList
    [ ("Browser", browser)
    , ("Html", html)
    , ("Http", http)
    , ("Json.Decode", json)
    , ("Json.Encode", json)
    , ("Url.Parser", url)
    , ("Url", url)
    ]



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


versionFromText :: Text -> Maybe Version
versionFromText text =
  case Text.splitOn "." text of
    [major, minor, patch] ->
      Version
        <$> toNumber major
        <*> toNumber minor
        <*> toNumber patch

    _ ->
      Nothing


toNumber :: Text -> Maybe Word16
toNumber txt =
  case Text.decimal txt of
    Right (n, "") ->
      Just n

    _ ->
      Nothing



-- BINARY


instance Binary Name where
  get =
    liftM2 Name get get

  put (Name author project) =
    do  put author
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


decoder :: Decode.Decoder String Name
decoder =
  do  txt <- Decode.text
      case fromText txt of
        Right name ->
          Decode.succeed name

        Left (msg, _) ->
          Decode.fail msg



encode :: Name -> Encode.Value
encode name =
  Encode.text (toText name)



versionDecoder :: Decode.Decoder Text Version
versionDecoder =
  do  txt <- Decode.text
      case versionFromText txt of
        Just version ->
          Decode.succeed version

        Nothing ->
          Decode.fail txt


encodeVersion :: Version -> Encode.Value
encodeVersion version =
  Encode.text (versionToText version)
