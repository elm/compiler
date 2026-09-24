{-# LANGUAGE BangPatterns, ExtendedLiterals, QuasiQuotes, MagicHash, UnboxedTuples #-}
module Elm.Package
  ( Name(..)
  , Canonical(..)
  , isKernel
  , toChars
  , toUrl
  , toFilePath
  , toJsonString
  , toJavaScriptBuilder
  --
  , dummyName, kernel, core
  , browser, virtualDom, html
  , json, http, url
  , webgl, linearAlgebra
  --
  , suggestions
  , nearbyNames
  --
  , dName, eName
  , dCanonical, eCanonical
  --
  , decoder
  , encode
  , keyDecoder
  --
  , parser
  )
  where


import Control.Monad (liftM2)
import qualified Data.ByteString.Builder as B
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Utf8 as Utf8
import GHC.Exts (isTrue#)
import GHC.Prim
import System.FilePath ((</>))

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified String as S
import qualified ThreadSafe.Fork as Fork

import qualified AST.Prim.Module as Module
import qualified Elm.Version as V
import qualified Json.Decode as JD
import qualified Json.Encode as JE
import qualified Json.String as Json
import qualified Parse.Primitives as P
import Parse.Primitives (Cursor)
import qualified Reporting.Annotation as A
import qualified Reporting.Suggest as Suggest



-- PACKGE NAMES


data Name = Name Author Project


newtype Author  = Author  S.String
newtype Project = Project S.String


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
  S.toChars (coerce author) <> "/" <> S.toChars (coerce project)


toUrl :: Name -> String
toUrl (Name author project) =
  S.toChars (coerce author) ++ "/" ++ S.toChars (coerce project)


toFilePath :: Name -> FilePath
toFilePath (Name author project) =
  S.toChars (coerce author) </> S.toChars (coerce project)


toJsonString :: Name -> Json.String
toJsonString (Name author project) =
    Utf8.Utf8 ba
  where
    !(S.String ba) = S.join (coerce author) 0x2F#Word8 {-/-} (coerce project)


toJavaScriptBuilder :: Name -> B.Builder
toJavaScriptBuilder (Name author project) =
  S.toEscapedBuilder 0x2D#Word8 {---} 0x5F#Word8 {-_-} (coerce author)
  <> B.word8 0x24 {-$-} <>
  S.toEscapedBuilder 0x2D#Word8 {---} 0x5F#Word8 {-_-} (coerce project)



-- COMMON PACKAGE NAMES


toName :: Author -> S.String -> Name
toName author project =
  Name author (Project project)


{-# NOINLINE dummyName #-}
dummyName :: Name
dummyName =
  toName (Author [S.ascii|author|]) [S.ascii|project|]


kernel        :: Name; kernel        = toName elm              [S.ascii|kernel|]
core          :: Name; core          = toName elm              [S.ascii|core|]
browser       :: Name; browser       = toName elm              [S.ascii|browser|]
virtualDom    :: Name; virtualDom    = toName elm              [S.ascii|virtual-dom|]
html          :: Name; html          = toName elm              [S.ascii|html|]
json          :: Name; json          = toName elm              [S.ascii|json|]
http          :: Name; http          = toName elm              [S.ascii|http|]
url           :: Name; url           = toName elm              [S.ascii|url|]
webgl         :: Name; webgl         = toName elm_explorations [S.ascii|webgl|]
linearAlgebra :: Name; linearAlgebra = toName elm_explorations [S.ascii|linear-algebra|]


elm              :: Author; elm              = Author [S.ascii|elm|]
elm_explorations :: Author; elm_explorations = Author [S.ascii|elm-explorations|]



-- PACKAGE SUGGESTIONS


suggestions :: Map.Map Module.Name Name
suggestions =
  let
    random = toName elm [S.ascii|random|]
    time   = toName elm [S.ascii|time|]
    file   = toName elm [S.ascii|file|]
  in
  Map.fromList
    [ [S.ascii|Browser|]         ==> browser
    , [S.ascii|File|]            ==> file
    , [S.ascii|File.Download|]   ==> file
    , [S.ascii|File.Select|]     ==> file
    , [S.ascii|Html|]            ==> html
    , [S.ascii|Html.Attributes|] ==> html
    , [S.ascii|Html.Events|]     ==> html
    , [S.ascii|Http|]            ==> http
    , [S.ascii|Json.Decode|]     ==> json
    , [S.ascii|Json.Encode|]     ==> json
    , [S.ascii|Random|]          ==> random
    , [S.ascii|Time|]            ==> time
    , [S.ascii|Url.Parser|]      ==> url
    , [S.ascii|Url|]             ==> url
    ]


(==>) :: S.String -> Name -> (Module.Name, Name)
(==>) moduleName package =
  ( Module.fromString moduleName, package )



-- NEARBY NAMES


nearbyNames :: Name -> [Name] -> [Name]
nearbyNames (Name author1 project1) possibleNames =
  let
    authorDist  = authorDistance  $ S.toChars $ coerce author1
    projectDist = projectDistance $ S.toChars $ coerce project1

    nameDistance (Name author2 project2) =
      authorDist author2 + projectDist project2
  in
  take 4 $ List.sortOn nameDistance possibleNames


authorDistance :: [Char] -> Author -> Int
authorDistance given possibility =
  if possibility == elm || possibility == elm_explorations
  then 0
  else abs $ Suggest.distance given $ S.toChars (coerce possibility)


projectDistance :: [Char] -> Project -> Int
projectDistance given possibility =
  abs $ Suggest.distance given $ S.toChars (coerce possibility)



-- INSTANCES


instance Eq  Name where (==)    (Name a p) (Name a' p') = p == p' && a == a'
instance Ord Name where compare (Name a p) (Name a' p') = case compare a a' of { EQ -> compare p p' ; ne -> ne }

instance Eq Canonical where
  (==) (Canonical package1 version1) (Canonical package2 version2) =
    version1 == version2 && package1 == package2

instance Fork.Context Name where
  toContextChars = toChars

instance Eq  Author where (==)    x y = S.equal          (coerce x) (coerce y)
instance Ord Author where compare x y = S.compareLexical (coerce x) (coerce y)

instance Eq  Project where (==)    x y = S.equal          (coerce x) (coerce y)
instance Ord Project where compare x y = S.compareLexical (coerce x) (coerce y)



-- BINARY


dName :: D.Decoder Name
dName =
  liftM2 Name (coerce D.string8) (coerce D.string8)


eName :: Name -> E.Builder
eName (Name a p) =
  E.string8 (coerce a) <> E.string8 (coerce p)


dCanonical :: D.Decoder Canonical
dCanonical =
  liftM2 Canonical dName V.dVersion


eCanonical :: Canonical -> E.Builder
eCanonical (Canonical n v) =
  eName n <> V.eVersion v



-- JSON


decoder :: JD.Decoder A.Position Name
decoder =
  JD.customString parser A.Position


encode :: Name -> JE.Value
encode name =
  JE.chars (toChars name)


keyDecoder :: (Cursor -> x) -> JD.KeyDecoder x Name
keyDecoder toError =
  let
    keyParser =
      P.specialize (\(A.Position c) _ -> toError c) parser
  in
  JD.KeyDecoder keyParser toError



-- PARSER


parser :: P.Parser A.Position Name
parser =
  do  author <- parseName isAlphaOrDigit isAlphaOrDigit
      P.word1 0x2F#Word8 {-/-} A.Position
      project <- parseName isLower isLowerOrDigit
      return $ Name (Author author) (Project project)


parseName :: (Word8# -> Bool) -> (Word8# -> Bool) -> P.Parser A.Position S.String
parseName isGoodStart isGoodInner =
  P.Parser $ \_ (P.State pos end indent cur) cok _ cerr eerr ->
    if P.notLtAddr pos end then
      eerr cur A.Position
    else
      let !word = indexWord8OffAddr# pos 0# in
      if not (isGoodStart word) then
        eerr cur A.Position
      else
        let
          !(# isGood, newPos #) = chompName isGoodInner (plusAddr# pos 1#) end False
          !len = minusAddr# newPos pos
          !newCur = P.slide cur (wordToWord64# (int2Word# len))
        in
        if isGood && isTrue# (len <# 256#) then
          do  let !newState = P.State newPos end indent newCur
              name <- S.fromAddr pos newPos
              cok name newState
        else
          cerr newCur A.Position


isLower :: Word8# -> Bool
isLower w =
  isBetween 0x61#Word8 w 0x7A#Word8 {-a-} {-z-}


isLowerOrDigit :: Word8# -> Bool
isLowerOrDigit w =
     isBetween 0x61#Word8 w 0x7A#Word8 {-a-} {-z-}
  || isBetween 0x30#Word8 w 0x39#Word8 {-0-} {-9-}


isAlphaOrDigit :: Word8# -> Bool
isAlphaOrDigit w =
     isBetween 0x61#Word8 w 0x7A#Word8 {-a-} {-z-}
  || isBetween 0x41#Word8 w 0x5A#Word8 {-A-} {-Z-}
  || isBetween 0x30#Word8 w 0x39#Word8 {-0-} {-9-}


{-# INLINE isBetween #-}
isBetween :: Word8# -> Word8# -> Word8# -> Bool
isBetween lo w hi =
  isTrue# (leWord8# lo w) && isTrue# (leWord8# w hi)


chompName :: (Word8# -> Bool) -> Addr# -> Addr# -> Bool -> (# Bool, Addr# #)
chompName isGoodChar pos end prevWasDash =
  if P.notLtAddr pos end then
    (# not prevWasDash, pos #)
  else
    let !word = indexWord8OffAddr# pos 0# in
    if isGoodChar word then
      chompName isGoodChar (plusAddr# pos 1#) end False
    else if isTrue# (eqWord8# word 0x2D#Word8 {---}) then
      if prevWasDash then
        (# False, pos #)
      else
        chompName isGoodChar (plusAddr# pos 1#) end True
    else
      (# True, pos #)
