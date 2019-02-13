module Elm.Version
  ( Version(..)
  , one
  , max
  , compiler
  , bumpPatch
  , bumpMinor
  , bumpMajor
  , toChars
  , toString
  , fromString
  , decoder
  , encode
  )
  where


import Prelude hiding (max)
import Control.Monad (liftM, liftM3)
import Data.Binary (Binary, get, put, getWord8, putWord8)
import qualified Data.Utf8 as Utf8
import qualified Data.Version as Version
import Data.Word (Word16)
import qualified Paths_elm

import qualified Json.Decode as D
import qualified Json.Encode as E



-- VERSION


data Version =
  Version
    { _major :: {-# UNPACK #-} !Word16
    , _minor :: {-# UNPACK #-} !Word16
    , _patch :: {-# UNPACK #-} !Word16
    }
    deriving (Eq, Ord)


one :: Version
one =
  Version 1 0 0


max :: Version
max =
  Version maxBound 0 0


compiler :: Version
compiler =
  case map fromIntegral (Version.versionBranch Paths_elm.version) of
    major : minor : patch : _ ->
      Version major minor patch

    [major, minor] ->
      Version major minor 0

    [major] ->
      Version major 0 0

    [] ->
      error "could not detect version of elm-compiler you are using"



-- BUMP


bumpPatch :: Version -> Version
bumpPatch (Version major minor patch) =
  Version major minor (patch + 1)


bumpMinor :: Version -> Version
bumpMinor (Version major minor _patch) =
  Version major (minor + 1) 0


bumpMajor :: Version -> Version
bumpMajor (Version major _minor _patch) =
  Version (major + 1) 0 0



-- TO STRING


toChars :: Version -> String
toChars vsn =
  Utf8.toChars (toString vsn)


toString :: Version -> Utf8.String
toString (Version major minor patch) =
  Utf8.join 0x2E {- . -}
    [ Utf8.fromWord16 major
    , Utf8.fromWord16 minor
    , Utf8.fromWord16 patch
    ]



-- FROM STRING


fromString :: Utf8.String -> Maybe Version
fromString str =
  case Utf8.split 0x2E {- . -} str of
    [major, minor, patch] ->
      Version
        <$> Utf8.toWord16 major
        <*> Utf8.toWord16 minor
        <*> Utf8.toWord16 patch

    _ ->
      Nothing



-- JSON


decoder :: D.Decoder Utf8.String Version
decoder =
  do  str <- D.string
      case fromString str of
        Just version ->
          return version

        Nothing ->
          D.failure str


encode :: Version -> E.Value
encode version =
  E.string (toString version)



-- BINARY


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

