{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, EmptyDataDecls, FlexibleInstances, UnboxedTuples #-}
module Elm.Package
  ( Name(..)
  , Author
  , Project
  , Canonical(..)
  , isKernel
  , toChars
  , toUrl
  , toFilePath
  , toJsonString
  --
  , dummyName, kernel, core
  , browser, virtualDom, html
  , json, http, url
  , webgl, linearAlgebra
  --
  , suggestions
  , nearbyNames
  --
  , decoder
  , encode
  , keyDecoder
  --
  , parser
  )
  where


import Control.Monad (liftM2)
import Data.Binary (Binary, get, put)
import qualified Data.Coerce as Coerce
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Name as Name
import Data.Monoid ((<>))
import qualified Data.Utf8 as Utf8
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr, minusPtr)
import System.FilePath ((</>))

import qualified Elm.Version as V
import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Json.String as Json
import qualified Parse.Primitives as P
import Parse.Primitives (Row, Col)
import qualified Reporting.Suggest as Suggest



-- PACKGE NAMES


data Name =
  Name
    { _author :: !Author
    , _project :: !Project
    }
    deriving (Ord)


type Author = Utf8.Utf8 AUTHOR
type Project = Utf8.Utf8 PROJECT

data AUTHOR
data PROJECT


data Canonical =
  Canonical
    { _name :: !Name
    , _version :: !V.Version
    }
    deriving (Ord)



-- HELPERS


isKernel :: Name -> Bool
isKernel (Name author _) =
  author == elm || author == elm_explorations


toChars :: Name -> String
toChars (Name author project) =
  Utf8.toChars author <> "/" <> Utf8.toChars project


toUrl :: Name -> String
toUrl (Name author project) =
  Utf8.toChars author ++ "/" ++ Utf8.toChars project


toFilePath :: Name -> FilePath
toFilePath (Name author project) =
  Utf8.toChars author </> Utf8.toChars project


toJsonString :: Name -> Json.String
toJsonString (Name author project) =
  Utf8.join 0x2F {-/-} [ Coerce.coerce author, Coerce.coerce project ]



-- COMMON PACKAGE NAMES


toName :: Author -> [Char] -> Name
toName author project =
  Name author (Utf8.fromChars project)


{-# NOINLINE dummyName #-}
dummyName :: Name
dummyName =
  toName (Utf8.fromChars "author") "project"


{-# NOINLINE kernel #-}
kernel :: Name
kernel =
  toName elm "kernel"


{-# NOINLINE core #-}
core :: Name
core =
  toName elm "core"


{-# NOINLINE browser #-}
browser :: Name
browser =
  toName elm "browser"


{-# NOINLINE virtualDom #-}
virtualDom :: Name
virtualDom =
  toName elm "virtual-dom"


{-# NOINLINE html #-}
html :: Name
html =
  toName elm "html"


{-# NOINLINE json #-}
json :: Name
json =
  toName elm "json"


{-# NOINLINE http #-}
http :: Name
http =
  toName elm "http"


{-# NOINLINE url #-}
url :: Name
url =
  toName elm "url"


{-# NOINLINE webgl #-}
webgl :: Name
webgl =
  toName elm_explorations "webgl"


{-# NOINLINE linearAlgebra #-}
linearAlgebra :: Name
linearAlgebra =
  toName elm_explorations "linear-algebra"


{-# NOINLINE elm #-}
elm :: Author
elm =
  Utf8.fromChars "elm"


{-# NOINLINE elm_explorations #-}
elm_explorations :: Author
elm_explorations =
  Utf8.fromChars "elm-explorations"



-- PACKAGE SUGGESTIONS


suggestions :: Map.Map Name.Name Name
suggestions =
  let
    random = toName elm "random"
    time = toName elm "time"
    file = toName elm "file"
  in
  Map.fromList
    [ "Browser" ==> browser
    , "File" ==> file
    , "File.Download" ==> file
    , "File.Select" ==> file
    , "Html" ==> html
    , "Html.Attributes" ==> html
    , "Html.Events" ==> html
    , "Http" ==> http
    , "Json.Decode" ==> json
    , "Json.Encode" ==> json
    , "Random" ==> random
    , "Time" ==> time
    , "Url.Parser" ==> url
    , "Url" ==> url
    ]


(==>) :: [Char] -> Name -> (Name.Name, Name)
(==>) moduleName package =
  ( Utf8.fromChars moduleName, package )



-- NEARBY NAMES


nearbyNames :: Name -> [Name] -> [Name]
nearbyNames (Name author1 project1) possibleNames =
  let
    authorDist = authorDistance (Utf8.toChars author1)
    projectDist = projectDistance (Utf8.toChars project1)

    nameDistance (Name author2 project2) =
      authorDist author2 + projectDist project2
  in
  take 4 $ List.sortOn nameDistance possibleNames


authorDistance :: [Char] -> Author -> Int
authorDistance given possibility =
  if possibility == elm || possibility == elm_explorations
  then 0
  else abs (Suggest.distance given (Utf8.toChars possibility))


projectDistance :: [Char] -> Project -> Int
projectDistance given possibility =
  abs (Suggest.distance given (Utf8.toChars possibility))



-- INSTANCES


instance Eq Name where
  (==) (Name author1 project1) (Name author2 project2) =
    project1 == project2 && author1 == author2


instance Eq Canonical where
  (==) (Canonical package1 version1) (Canonical package2 version2) =
    version1 == version2 && package1 == package2



-- BINARY


instance Binary Name where -- PERF try storing as a Word16
  get = liftM2 Name Utf8.getUnder256 Utf8.getUnder256
  put (Name a b) = Utf8.putUnder256 a >> Utf8.putUnder256 b


instance Binary Canonical where
  get = liftM2 Canonical get get
  put (Canonical a b) = put a >> put b



-- JSON


decoder :: D.Decoder (Row, Col) Name
decoder =
  D.customString parser (,)


encode :: Name -> E.Value
encode name =
  E.chars (toChars name)


keyDecoder :: (Row -> Col -> x) -> D.KeyDecoder x Name
keyDecoder toError =
  let
    keyParser =
      P.specialize (\(r,c) _ _ -> toError r c) parser
  in
  D.KeyDecoder keyParser toError



-- PARSER


parser :: P.Parser (Row, Col) Name
parser =
  do  author <- parseName isAlphaOrDigit isAlphaOrDigit
      P.word1 0x2F {-/-} (,)
      project <- parseName isLower isLowerOrDigit
      return (Name author project)


parseName :: (Word8 -> Bool) -> (Word8 -> Bool) -> P.Parser (Row, Col) (Utf8.Utf8 t)
parseName isGoodStart isGoodInner =
  P.Parser $ \(P.State src pos end indent row col) cok _ cerr eerr ->
    if pos >= end then
      eerr row col (,)
    else
      let !word = P.unsafeIndex pos in
      if not (isGoodStart word) then
        eerr row col (,)
      else
        let
          (# isGood, newPos #) = chompName isGoodInner (plusPtr pos 1) end False
          !len = fromIntegral (minusPtr newPos pos)
          !newCol = col + len
        in
        if isGood && len < 256 then
          let !newState = P.State src newPos end indent row newCol in
          cok (Utf8.fromPtr pos newPos) newState
        else
          cerr row newCol (,)


isLower :: Word8 -> Bool
isLower word =
  0x61 {-a-} <= word && word <= 0x7A {-z-}


isLowerOrDigit :: Word8 -> Bool
isLowerOrDigit word =
     0x61 {-a-} <= word && word <= 0x7A {-z-}
  || 0x30 {-0-} <= word && word <= 0x39 {-9-}


isAlphaOrDigit :: Word8 -> Bool
isAlphaOrDigit word =
     0x61 {-a-} <= word && word <= 0x7A {-z-}
  || 0x41 {-A-} <= word && word <= 0x5A {-Z-}
  || 0x30 {-0-} <= word && word <= 0x39 {-9-}


chompName :: (Word8 -> Bool) -> Ptr Word8 -> Ptr Word8 -> Bool -> (# Bool, Ptr Word8 #)
chompName isGoodChar pos end prevWasDash =
  if pos >= end then
    (# not prevWasDash, pos #)
  else
    let !word = P.unsafeIndex pos in
    if isGoodChar word then
      chompName isGoodChar (plusPtr pos 1) end False
    else if word == 0x2D {---} then
      if prevWasDash then
        (# False, pos #)
      else
        chompName isGoodChar (plusPtr pos 1) end True
    else
      (# True, pos #)
