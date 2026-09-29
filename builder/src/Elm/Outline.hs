{-# LANGUAGE ExtendedLiterals, MagicHash, MultiWayIf, OverloadedStrings, QuasiQuotes #-}
module Elm.Outline
  ( Outline(..)
  , AppOutline(..)
  , PkgOutline(..)
  , Exposed(..)
  , read
  , write
  , encode
  , decoder
  , defaultSummary
  , flattenExposed
  --
  , ePath, dPath
  )
  where


import Prelude hiding (read)
import Control.Monad (filterM)
import qualified Data.Map as Map
import qualified Data.NonEmptyList as NE
import qualified Data.OneOrMore as OneOrMore
import GHC.Exts (isTrue#)
import GHC.Prim
import qualified System.Directory as Dir
import qualified System.FilePath as FP

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified String as S

import qualified AST.Prim.Module as Module
import qualified Elm.Constraint as Con
import qualified Elm.Licenses as Licenses
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified File
import qualified Json.Decode as JD
import qualified Json.Encode as JE
import Json.Encode ((==>))
import qualified Json.String as Json
import qualified Parse.Primitives as P
import qualified Reporting.Exit as Exit
import qualified Root as R



-- OUTLINE


data Outline
  = App AppOutline
  | Pkg PkgOutline


data AppOutline =
  AppOutline
    { _app_elm_version :: V.Version
    , _app_source_dirs :: NE.List R.Path
    , _app_deps_direct :: Map.Map Pkg.Name V.Version
    , _app_deps_indirect :: Map.Map Pkg.Name V.Version
    , _app_test_direct :: Map.Map Pkg.Name V.Version
    , _app_test_indirect :: Map.Map Pkg.Name V.Version
    }


data PkgOutline =
  PkgOutline
    { _pkg_name :: Pkg.Name
    , _pkg_summary :: Json.String
    , _pkg_license :: Licenses.License
    , _pkg_version :: V.Version
    , _pkg_exposed :: Exposed
    , _pkg_deps :: Map.Map Pkg.Name Con.Constraint
    , _pkg_test_deps :: Map.Map Pkg.Name Con.Constraint
    , _pkg_elm_version :: Con.Constraint
    }


data Exposed
  = ExposedList [Module.Name]
  | ExposedDict [(Json.String, [Module.Name])]



-- DEFAULTS


defaultSummary :: Json.String
defaultSummary =
  Json.fromChars "helpful summary of your project, less than 80 characters"



-- HELPERS


flattenExposed :: Exposed -> [Module.Name]
flattenExposed exposed =
  case exposed of
    ExposedList names ->
      names

    ExposedDict sections ->
      concatMap snd sections



-- WRITE


write :: File.Writer t -> R.Root -> Outline -> IO ()
write writer root outline =
  JE.write writer (R.elm_json root) (encode outline)



-- JSON ENCODE


encode :: Outline -> JE.Value
encode outline =
  case outline of
    App (AppOutline elm srcDirs depsDirect depsTrans testDirect testTrans) ->
      JE.object
        [ "type" ==> JE.chars "application"
        , "source-directories" ==> JE.list encodePath (NE.toList srcDirs)
        , "elm-version" ==> V.encode elm
        , "dependencies" ==>
            JE.object
              [ "direct" ==> encodeDeps V.encode depsDirect
              , "indirect" ==> encodeDeps V.encode depsTrans
              ]
        , "test-dependencies" ==>
            JE.object
              [ "direct" ==> encodeDeps V.encode testDirect
              , "indirect" ==> encodeDeps V.encode testTrans
              ]
        ]

    Pkg (PkgOutline name summary license version exposed deps tests elm) ->
      JE.object
        [ "type" ==> JE.string [S.ascii|package|]
        , "name" ==> Pkg.encode name
        , "summary" ==> JE.jsonString summary
        , "license" ==> Licenses.encode license
        , "version" ==> V.encode version
        , "exposed-modules" ==> encodeExposed exposed
        , "elm-version" ==> Con.encode elm
        , "dependencies" ==> encodeDeps Con.encode deps
        , "test-dependencies" ==> encodeDeps Con.encode tests
        ]


encodeExposed :: Exposed -> JE.Value
encodeExposed exposed =
  case exposed of
    ExposedList modules ->
      JE.list Module.jsonEncodeName modules

    ExposedDict chunks ->
      JE.object (map (fmap (JE.list Module.jsonEncodeName)) chunks)


encodeDeps :: (a -> JE.Value) -> Map.Map Pkg.Name a -> JE.Value
encodeDeps encodeValue deps =
  JE.dict Pkg.toJsonString encodeValue deps


encodePath :: R.Path -> JE.Value
encodePath path =
  case path of
    R.Absolute dir -> JE.chars dir
    R.Relative dir -> JE.chars dir



-- PARSE AND VERIFY


read :: R.Root -> IO (Either Exit.Outline Outline)
read root =
  do  bytes <- File.readUtf8 (R.elm_json root)
      result <- JD.fromByteString decoder bytes
      case result of
        Left x ->
          return $ Left $ Exit.OutlineHasBadStructure x

        Right outline ->
          case outline of
            Pkg (PkgOutline pkg _ _ _ _ deps _ _) ->
              return $
                if Map.notMember Pkg.core deps && pkg /= Pkg.core
                then Left Exit.OutlineNoPkgCore
                else Right outline

            App (AppOutline _ srcDirs direct indirect _ _)
              | Map.notMember Pkg.core direct ->
                  return $ Left Exit.OutlineNoAppCore

              | Map.notMember Pkg.json direct && Map.notMember Pkg.json indirect ->
                  return $ Left Exit.OutlineNoAppJson

              | otherwise ->
                  do  badDirs <- filterM (isSrcDirMissing root) (NE.toList srcDirs)
                      case map R.toOriginalPath badDirs of
                        d:ds ->
                          return $ Left (Exit.OutlineHasMissingSrcDirs d ds)

                        [] ->
                          do  maybeDups <- detectDuplicates root (NE.toList srcDirs)
                              case maybeDups of
                                Nothing ->
                                  return $ Right outline

                                Just (canonicalDir, (dir1,dir2)) ->
                                  return $ Left (Exit.OutlineHasDuplicateSrcDirs canonicalDir dir1 dir2)


isSrcDirMissing :: R.Root -> R.Path -> IO Bool
isSrcDirMissing root srcDir =
  not <$> Dir.doesDirectoryExist (R.toAbsolutePath root srcDir)


detectDuplicates :: R.Root -> [R.Path] -> IO (Maybe (FilePath, (FilePath, FilePath)))
detectDuplicates root srcDirs =
  do  pairs <- traverse (toPair root) srcDirs
      return $ Map.lookupMin $ Map.mapMaybe isDup $
        Map.fromListWith OneOrMore.more pairs


toPair :: R.Root -> R.Path -> IO (FilePath, OneOrMore.OneOrMore FilePath)
toPair root srcDir =
  do  key <- Dir.canonicalizePath (R.toAbsolutePath root srcDir)
      return (key, OneOrMore.one (R.toOriginalPath srcDir))


isDup :: OneOrMore.OneOrMore FilePath -> Maybe (FilePath, FilePath)
isDup paths =
  case paths of
    OneOrMore.One _    -> Nothing
    OneOrMore.More a b -> Just (OneOrMore.getFirstTwo a b)



-- JSON DECODE


type Decoder a =
  JD.Decoder Exit.OutlineProblem a


decoder :: Decoder Outline
decoder =
  let
    application = Json.fromChars "application"
    package     = Json.fromChars "package"
  in
  do  tipe <- JD.field "type" JD.jsonString
      if  | tipe == application -> App <$> appDecoder
          | tipe == package     -> Pkg <$> pkgDecoder
          | otherwise           -> JD.failure Exit.OP_BadType


appDecoder :: Decoder AppOutline
appDecoder =
  AppOutline
    <$> JD.field "elm-version" versionDecoder
    <*> JD.field "source-directories" dirsDecoder
    <*> JD.field "dependencies" (JD.field "direct" (depsDecoder versionDecoder))
    <*> JD.field "dependencies" (JD.field "indirect" (depsDecoder versionDecoder))
    <*> JD.field "test-dependencies" (JD.field "direct" (depsDecoder versionDecoder))
    <*> JD.field "test-dependencies" (JD.field "indirect" (depsDecoder versionDecoder))


pkgDecoder :: Decoder PkgOutline
pkgDecoder =
  PkgOutline
    <$> JD.field "name" nameDecoder
    <*> JD.field "summary" summaryDecoder
    <*> JD.field "license" (Licenses.decoder Exit.OP_BadLicense)
    <*> JD.field "version" versionDecoder
    <*> JD.field "exposed-modules" exposedDecoder
    <*> JD.field "dependencies" (depsDecoder constraintDecoder)
    <*> JD.field "test-dependencies" (depsDecoder constraintDecoder)
    <*> JD.field "elm-version" constraintDecoder



-- JSON DECODE HELPERS


nameDecoder :: Decoder Pkg.Name
nameDecoder =
  JD.mapError Exit.OP_BadPkgName Pkg.decoder


summaryDecoder :: Decoder Json.String
summaryDecoder =
  JD.customString
    (boundParser 80# Exit.OP_BadSummaryTooLong)
    (\_ -> Exit.OP_BadSummaryTooLong)


versionDecoder :: Decoder V.Version
versionDecoder =
  JD.mapError Exit.OP_BadVersion V.decoder


constraintDecoder :: Decoder Con.Constraint
constraintDecoder =
  JD.mapError Exit.OP_BadConstraint Con.decoder


depsDecoder :: Decoder a -> Decoder (Map.Map Pkg.Name a)
depsDecoder valueDecoder =
  JD.dict (Pkg.keyDecoder Exit.OP_BadDependencyName) valueDecoder


dirsDecoder :: Decoder (NE.List R.Path)
dirsDecoder =
  fmap (toSrcDir . Json.toChars) <$> JD.nonEmptyList JD.jsonString Exit.OP_NoSrcDirs


toSrcDir :: FilePath -> R.Path
toSrcDir path =
  if FP.isRelative path
  then R.Relative path
  else R.Absolute path



-- EXPOSED MODULES DECODER


exposedDecoder :: Decoder Exposed
exposedDecoder =
  JD.oneOf
    [ ExposedList <$> JD.list moduleDecoder
    , ExposedDict <$> JD.pairs headerKeyDecoder (JD.list moduleDecoder)
    ]


moduleDecoder :: Decoder Module.Name
moduleDecoder =
  Module.jsonDecodeName Exit.OP_BadModuleName


headerKeyDecoder :: JD.KeyDecoder Exit.OutlineProblem Json.String
headerKeyDecoder =
  JD.KeyDecoder
    (boundParser 20# Exit.OP_BadModuleHeaderTooLong)
    (\_ -> Exit.OP_BadModuleHeaderTooLong)



-- BOUND PARSER


boundParser :: Int# -> x -> P.Parser x Json.String
boundParser bound tooLong =
  P.Parser $ \_ (P.State pos end indent cur) cok _ cerr _ ->
    let
      len = minusAddr# end pos
      newCur = P.slide cur (wordToWord64# (int2Word# len))
    in
    if isTrue# (len <# bound)
    then
      do  str <- Json.fromAddr pos end
          cok str (P.State end end indent newCur)
    else
      cerr newCur (\_ -> tooLong)



-- BINARY


ePath :: R.Path -> E.Builder
ePath srcDir =
  case srcDir of
    R.Absolute d -> E.u8# 0#Word8 <> E.chars64 d
    R.Relative d -> E.u8# 1#Word8 <> E.chars64 d


dPath :: D.Decoder R.Path
dPath =
  do  tag <- D.u8
      case tag of
        0 -> R.Absolute <$> D.chars64
        1 -> R.Relative <$> D.chars64
        _ -> D.expecting "R.Path"
