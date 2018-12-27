{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Project.Json
  ( Project(..)
  , AppInfo(..)
  , PkgInfo(..)
  , Exposed(..)
  , defaultSummary
  -- json
  , write
  , encode
  , read
  , pkgDecoder
  -- queries
  , isPlatformPackage
  , getName
  , getExposed
  , check
  )
  where


import Prelude hiding (read)
import Control.Monad.Trans (liftIO)
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import qualified Data.Utf8 as Utf8
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Elm.Constraint as Con
import qualified Elm.Licenses as Licenses
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Assets as E
import qualified Reporting.Task as Task



-- PROJECT


data Project
  = App AppInfo
  | Pkg PkgInfo



-- APPLICATION


data AppInfo =
  AppInfo
    { _app_elm_version :: V.Version
    , _app_source_dirs :: [FilePath]
    , _app_deps_direct :: Map.Map Pkg.Name V.Version
    , _app_deps_trans :: Map.Map Pkg.Name V.Version
    , _app_test_direct :: Map.Map Pkg.Name V.Version
    , _app_test_trans :: Map.Map Pkg.Name V.Version
    }



-- PACKAGE


data PkgInfo =
  PkgInfo
    { _pkg_name :: Pkg.Name
    , _pkg_summary :: Utf8.String
    , _pkg_license :: Licenses.License
    , _pkg_version :: V.Version
    , _pkg_exposed :: Exposed
    , _pkg_deps :: Map.Map Pkg.Name Con.Constraint
    , _pkg_test_deps :: Map.Map Pkg.Name Con.Constraint
    , _pkg_elm_version :: Con.Constraint
    }


data Exposed
  = ExposedList [ModuleName.Raw]
  | ExposedDict [(Utf8.String, [ModuleName.Raw])]



-- DEFAULTS


defaultSummary :: Utf8.String
defaultSummary =
  "helpful summary of your project, less than 80 characters"



-- QUERIES


isPlatformPackage :: Project -> Bool
isPlatformPackage project =
  case project of
    App _ ->
      False

    Pkg info ->
      let
        (Pkg.Name author _) =
          _pkg_name info
      in
      author == "elm" || author == "elm-explorations"


getName :: Project -> Pkg.Name
getName project =
  case project of
    App _ ->
      Pkg.dummyName

    Pkg info ->
      _pkg_name info


getExposed :: PkgInfo -> [ModuleName.Raw]
getExposed info =
  case _pkg_exposed info of
    ExposedList modules ->
      modules

    ExposedDict chunks ->
      concatMap snd chunks


check :: Project -> Task.Task ()
check project =
  case project of
    Pkg (PkgInfo name _ _ _ _ deps _ _) ->
      if name == Pkg.core || Map.member Pkg.core deps
        then return ()
        else throwBadJson E.NoPkgCore

    App (AppInfo _ _ direct indirect _ _) ->
      if Map.member Pkg.core direct then

        if Map.member Pkg.json direct || Map.member Pkg.json indirect
          then return ()
          else throwBadJson E.NoAppJson

      else
        throwBadJson E.NoAppCore



-- WRITE


write :: FilePath -> Project -> IO ()
write root project =
  E.write (root </> "elm.json") (encode project)



-- JSON ENCODE


encode :: Project -> E.Value
encode project =
  case project of
    App (AppInfo elm srcDirs depsDirect depsTrans testDirect testTrans) ->
      E.object
        [ "type" ==> E.string "application"
        , "source-directories" ==> E.list (E.string . Utf8.fromChars) srcDirs
        , "elm-version" ==> V.encode elm
        , "dependencies" ==>
            E.object
              [ "direct" ==> encodeDeps V.encode depsDirect
              , "indirect" ==> encodeDeps V.encode depsTrans
              ]
        , "test-dependencies" ==>
            E.object
              [ "direct" ==> encodeDeps V.encode testDirect
              , "indirect" ==> encodeDeps V.encode testTrans
              ]
        ]

    Pkg (PkgInfo name summary license version exposed deps tests elm) ->
      E.object
        [ "type" ==> E.string "package"
        , "name" ==> E.string (Pkg.toString name)
        , "summary" ==> E.string summary
        , "license" ==> Licenses.encode license
        , "version" ==> V.encode version
        , "exposed-modules" ==> encodeExposed exposed
        , "elm-version" ==> Con.encode elm
        , "dependencies" ==> encodeDeps Con.encode deps
        , "test-dependencies" ==> encodeDeps Con.encode tests
        ]


(==>) :: a -> b -> (a, b)
(==>) a b =
  (a, b)


encodeExposed :: Exposed -> E.Value
encodeExposed exposed =
  case exposed of
    ExposedList modules ->
      E.list encodeModule modules

    ExposedDict chunks ->
      E.object (map (fmap (E.list encodeModule)) chunks)


encodeModule :: ModuleName.Raw -> E.Value
encodeModule name =
  E.name name


encodeDeps :: (a -> E.Value) -> Map.Map Pkg.Name a -> E.Value
encodeDeps encodeValue deps =
  E.dict Pkg.toString encodeValue deps



-- PARSE AND VERIFY


read :: FilePath -> Task.Task Project
read path =
  do  result <- liftIO $ D.fromFile decoder path
      case result of
        Right project ->
          case project of
            Pkg _ ->
              do  return project

            App (AppInfo _ srcDirs _ _ _ _) ->
              do  mapM_ doesDirectoryExist srcDirs
                  return project

        Left err ->
          throwBadJson (error "TODO bad json" err)


throwBadJson :: E.ElmJsonProblem -> Task.Task a
throwBadJson problem =
  Task.throw (Exit.Assets (E.BadElmJson problem))


doesDirectoryExist :: FilePath -> Task.Task ()
doesDirectoryExist dir =
  do  exists <- liftIO $ Dir.doesDirectoryExist dir
      if exists
        then return ()
        else throwBadJson (E.BadSrcDir dir)



-- JSON DECODE


type Decoder a =
  D.Decoder E.BadElmJsonContent a


decoder :: Decoder Project
decoder =
  do  tipe <- D.field "type" D.string
      case tipe of
        "application" ->
          App <$> appDecoder

        "package" ->
          Pkg <$> pkgDecoder

        other ->
          D.failure (E.BadType other)


appDecoder :: Decoder AppInfo
appDecoder =
  AppInfo
    <$> D.field "elm-version" versionDecoder
    <*> D.field "source-directories" (D.list dirDecoder)
    <*> D.field "dependencies" (D.field "direct" (depsDecoder versionDecoder))
    <*> D.field "dependencies" (D.field "indirect" (depsDecoder versionDecoder))
    <*> D.field "test-dependencies" (D.field "direct" (depsDecoder versionDecoder))
    <*> D.field "test-dependencies" (D.field "indirect" (depsDecoder versionDecoder))


pkgDecoder :: Decoder PkgInfo
pkgDecoder =
  PkgInfo
    <$> D.field "name" pkgNameDecoder
    <*> D.field "summary" summaryDecoder
    <*> D.field "license" licenseDecoder
    <*> D.field "version" versionDecoder
    <*> D.field "exposed-modules" exposedDecoder
    <*> D.field "dependencies" (depsDecoder constraintDecoder)
    <*> D.field "test-dependencies" (depsDecoder constraintDecoder)
    <*> D.field "elm-version" constraintDecoder



-- JSON DECODE HELPERS


pkgNameDecoder :: Decoder Pkg.Name
pkgNameDecoder =
  D.mapError E.BadPkgName Pkg.decoder


summaryDecoder :: Decoder Utf8.String
summaryDecoder =
  do  summary <- D.string
      if Utf8.size summary < 80
        then return summary
        else D.failure E.BadSummaryTooLong


licenseDecoder :: Decoder Licenses.License
licenseDecoder =
  Licenses.decoder E.BadLicense


versionDecoder :: Decoder V.Version
versionDecoder =
  D.mapError E.BadVersion V.decoder


constraintDecoder :: Decoder Con.Constraint
constraintDecoder =
  D.mapError E.BadConstraint Con.decoder


depsDecoder :: Decoder a -> Decoder (Map.Map Pkg.Name a)
depsDecoder valueDecoder =
  Map.fromList <$> (
    traverse validateKey =<< D.pairs valueDecoder
  )


validateKey :: (Utf8.String, a) -> Decoder (Pkg.Name, a)
validateKey (key, value) =
  case Pkg.fromString key of
    Right name ->
      return (name, value)

    Left _ ->
      D.failure (E.BadDependencyName key)


dirDecoder :: Decoder FilePath
dirDecoder =
  Utf8.toChars <$> D.string



-- EXPOSED MODULES DECODER


exposedDecoder :: Decoder Exposed
exposedDecoder =
  D.oneOf
    [ ExposedList <$> D.list moduleDecoder
    , do  pairs <- D.pairs (D.list moduleDecoder)
          traverse_ (checkHeader . fst) pairs
          return (ExposedDict pairs)
    ]


moduleDecoder :: Decoder ModuleName.Raw
moduleDecoder =
  D.mapError E.BadModuleName ModuleName.decoder


checkHeader :: Utf8.String -> Decoder ()
checkHeader header =
  if Utf8.size header < 20 then
    return ()
  else
    D.failure (E.BadModuleHeaderTooLong header)
