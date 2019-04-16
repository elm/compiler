{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Package
  ( Name(..)
  , Canonical(..)
  , isKernel
  , toString
  , toChars
  , toUrl
  , toFilePath
  , BadName(..)
  , fromString
  , dummyName, kernel, core
  , browser, virtualDom, html
  , json, http, url
  , webgl, linearAlgebra
  , suggestions
  , decoder
  , encode
  )
  where


import Control.Monad (liftM2)
import Data.Binary (Binary, get, put)
import qualified Data.Char as Char
import qualified Data.Name as Name
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Utf8 as Utf8
import System.FilePath ((</>))

import qualified Elm.Version as V
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode



-- PACKGE NAMES


data Name =
  Name
    { _author :: !Name.Name
    , _project :: !Name.Name
    }


data Canonical =
  Canonical
    { _name :: !Name
    , _version :: !V.Version
    }
    deriving (Ord)



-- HELPERS


isKernel :: Name -> Bool
isKernel (Name author _) =
  author == "elm" || author == "elm-explorations"


toString :: Name -> Utf8.String
toString (Name author project) =
  Utf8.join 0x2F [ Name.toUtf8 author, Name.toUtf8 project ]


toChars :: Name -> String
toChars (Name author project) =
  Name.toChars author <> "/" <> Name.toChars project


toUrl :: Name -> String
toUrl (Name author project) =
  Name.toChars author ++ "/" ++ Name.toChars project


toFilePath :: Name -> FilePath
toFilePath (Name author project) =
  Name.toChars author </> Name.toChars project


fromString :: Utf8.String -> Either BadName Name
fromString str =
  case Utf8.split 0x2F {- / -} str of
    [author, project]
      | Utf8.isEmpty author                      -> Left BadStructure
      | Utf8.isEmpty project                     -> Left BadStructure
      | Utf8.size project >= 256                 -> Left TooLong
      | Utf8.containsDouble 0x2D {- - -} project -> Left HasDoubleDash
      | Utf8.contains       0x5F {- _ -} project -> Left HasUnderscore
      | Utf8.contains       0x2E {- . -} project -> Left HasDot
      | Utf8.any Char.isUpper            project -> Left HasUppercase
      | Utf8.endsWithWord   0x2D {- - -} project -> Left HasDashEnd
      | Utf8.startsWithChar Char.isLower project -> Right (Name (Name.fromUtf8 author) (Name.fromUtf8 project))
      | True                                     -> Left HasNonLowercaseStart

    _ ->
      Left BadStructure


data BadName
  = BadStructure
  | TooLong
  | HasDoubleDash
  | HasUnderscore
  | HasDot
  | HasUppercase
  | HasDashEnd
  | HasNonLowercaseStart



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


suggestions :: Map.Map Name.Name Name
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



-- INSTANCES


instance Eq Name where
  (==) (Name author1 project1) (Name author2 project2) =
    project1 == project2 && author1 == author2


instance Eq Canonical where
  (==) (Canonical package1 version1) (Canonical package2 version2) =
    version1 == version2 && package1 == package2


instance Ord Name where
  compare (Name author1 project1) (Name author2 project2) =
    case compare project1 project2 of
      LT -> LT
      EQ -> compare author1 author2
      GT -> GT


instance Binary Name where
  get = liftM2 Name get get
  put (Name a b) = put a >> put b


instance Binary Canonical where
  get = liftM2 Canonical get get
  put (Canonical a b) = put a >> put b



-- JSON


decoder :: Decode.Decoder BadName Name
decoder =
  do  str <- Decode.string
      case fromString str of
        Right name ->
          return name

        Left err ->
          Decode.failure err


encode :: Name -> Encode.Value
encode name =
  Encode.string (toString name)
