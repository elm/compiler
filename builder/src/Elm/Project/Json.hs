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
  , appSolution
  , isPlatformPackage
  , get
  , getName
  , getExposed
  )
  where


import Prelude hiding (read)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString as BS
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import Elm.Package (Name, Version)

import qualified Elm.Project.Constraint as Con
import qualified Elm.Project.Licenses as Licenses
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
    { _app_elm_version :: Version
    , _app_source_dirs :: [FilePath]
    , _app_deps :: Map Name Version
    , _app_test_deps :: Map Name Version
    , _app_trans_deps :: Map Name Version
    }



-- PACKAGE


data PkgInfo =
  PkgInfo
    { _pkg_name :: Name
    , _pkg_summary :: Text
    , _pkg_license :: Licenses.License
    , _pkg_version :: Version
    , _pkg_exposed :: Exposed
    , _pkg_deps :: Map Name Con.Constraint
    , _pkg_test_deps :: Map Name Con.Constraint
    , _pkg_elm_version :: Con.Constraint
    }


data Exposed
  = ExposedList [Module.Raw]
  | ExposedDict [(Text, [Module.Raw])]



-- DEFAULTS


defaultSummary :: Text
defaultSummary =
  "helpful summary of your project, less than 80 characters"



-- QUERIES


appSolution :: AppInfo -> Map Name Version
appSolution info =
  Map.unions
    [ _app_deps info
    , _app_test_deps info
    , _app_trans_deps info
    ]


isPlatformPackage :: Project -> Bool
isPlatformPackage project =
  case project of
    App _ ->
      False

    Pkg info ->
      let
        (Pkg.Name user _) =
          _pkg_name info
      in
      user == "elm" || user == "elm-explorations"


get :: (AppInfo -> a) -> (PkgInfo -> a) -> Project -> a
get appFunc pkgFunc project =
  case project of
    App info ->
      appFunc info

    Pkg info ->
      pkgFunc info


getName :: Project -> Name
getName project =
  get (\_ -> Pkg.dummyName) _pkg_name project


getExposed :: PkgInfo -> [Module.Raw]
getExposed info =
  case _pkg_exposed info of
    ExposedList modules ->
      modules

    ExposedDict chunks ->
      concatMap snd chunks



-- WRITE


write :: FilePath -> Project -> IO ()
write root project =
  E.write (root </> "elm.json") (encode project)



-- JSON ENCODE


encode :: Project -> E.Value
encode project =
  case project of
    App (AppInfo elm srcDirs deps tests trans) ->
      E.object
        [ "type" ==> E.text "application"
        , "source-directories" ==> E.list (E.text . Text.pack) srcDirs
        , "elm-version" ==> encodeVersion elm
        , "dependencies" ==> encodeDeps encodeVersion deps
        , "test-dependencies" ==> encodeDeps encodeVersion tests
        , "do-not-edit-this-by-hand" ==>
            E.object [ "transitive-dependencies" ==> encodeDeps encodeVersion trans ]
        ]

    Pkg (PkgInfo name summary license version exposed deps tests elm) ->
      E.object
        [ "type" ==> E.text "package"
        , "name" ==> E.text (Pkg.toText name)
        , "summary" ==> E.text summary
        , "license" ==> Licenses.encode license
        , "version" ==> E.text (Pkg.versionToText version)
        , "exposed-modules" ==> encodeExposed exposed
        , "elm-version" ==> encodeConstraint elm
        , "dependencies" ==> encodeDeps encodeConstraint deps
        , "test-dependencies" ==> encodeDeps encodeConstraint tests
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


encodeModule :: Module.Raw -> E.Value
encodeModule name =
  E.name name


encodeDeps :: (a -> E.Value) -> Map Pkg.Name a -> E.Value
encodeDeps encodeValue deps =
  E.dict Pkg.toText encodeValue deps


encodeConstraint :: Con.Constraint -> E.Value
encodeConstraint constraint =
  E.text (Con.toText constraint)


encodeVersion :: Pkg.Version -> E.Value
encodeVersion version =
  E.text (Pkg.versionToText version)



-- PARSE AND VERIFY


read :: FilePath -> Task.Task Project
read path =
  do  bytes <- liftIO $ BS.readFile path
      case D.parse "project" E.badContentToDocs decoder bytes of
        Left err ->
          throwBadJson (E.BadJson err)

        Right project@(Pkg (PkgInfo name _ _ _ _ deps tests _)) ->
          do  checkOverlap "dependencies" "test-dependencies" deps tests
              pkgHasCore name deps
              return project

        Right project@(App (AppInfo _ srcDirs deps tests trans)) ->
          do  checkOverlap "dependencies" "test-dependencies" deps tests
              checkOverlap "dependencies" "transitive-dependencies" deps trans
              checkOverlap "test-dependencies" "transitive-dependencies" tests trans
              mapM_ doesDirectoryExist srcDirs
              appHasCoreAndJson (Map.union deps trans)
              return project


throwBadJson :: E.ElmJsonProblem -> Task.Task a
throwBadJson problem =
  Task.throw (Exit.Assets (E.BadElmJson problem))


checkOverlap :: String -> String -> Map Name a -> Map Name a -> Task.Task ()
checkOverlap field1 field2 deps1 deps2 =
  case Map.keys (Map.intersection deps1 deps2) of
    [] ->
      return ()

    dup : dups ->
      throwBadJson (E.BadDepDup field1 field2 dup dups)


doesDirectoryExist :: FilePath -> Task.Task ()
doesDirectoryExist dir =
  do  exists <- liftIO $ Dir.doesDirectoryExist dir
      if exists
        then return ()
        else throwBadJson (E.BadSrcDir dir)


appHasCoreAndJson :: Map Name a -> Task.Task ()
appHasCoreAndJson pkgs =
  if Map.member Pkg.core pkgs then

    if Map.member Pkg.json pkgs
      then return ()
      else throwBadJson E.NoAppJson

  else
    throwBadJson E.NoAppCore


pkgHasCore :: Name -> Map Name a -> Task.Task ()
pkgHasCore name pkgs =
  if name == Pkg.core || Map.member Pkg.core pkgs
    then return ()
    else throwBadJson E.NoPkgCore



-- JSON DECODE


type Decoder a =
  D.Decoder E.BadElmJsonContent a


decoder :: Decoder Project
decoder =
  do  tipe <- D.field "type" D.text
      case tipe of
        "application" ->
          D.map App appDecoder

        "package" ->
          D.map Pkg pkgDecoder

        other ->
          D.fail (E.BadType other)


appDecoder :: Decoder AppInfo
appDecoder =
  AppInfo
    <$> D.field "elm-version" versionDecoder
    <*> D.field "source-directories" (D.list dirDecoder)
    <*> D.field "dependencies" (depsDecoder versionDecoder)
    <*> D.field "test-dependencies" (depsDecoder versionDecoder)
    <*> D.at ["do-not-edit-this-by-hand", "transitive-dependencies"] (depsDecoder versionDecoder)


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


summaryDecoder :: Decoder Text
summaryDecoder =
  do  summary <- D.text
      if Text.length summary < 80
        then D.succeed summary
        else D.fail E.BadSummaryTooLong


licenseDecoder :: Decoder Licenses.License
licenseDecoder =
  do  txt <- D.text
      case Licenses.check txt of
        Left suggestions ->
          D.fail (E.BadLicense txt suggestions)

        Right license ->
          D.succeed license


versionDecoder :: Decoder Version
versionDecoder =
  do  txt <- D.text
      case Pkg.versionFromText txt of
        Just version ->
          D.succeed version

        Nothing ->
          D.fail (E.BadVersion txt)


constraintDecoder :: Decoder Con.Constraint
constraintDecoder =
  do  txt <- D.text
      case Con.fromText txt of
        Right constraint ->
          D.succeed constraint

        Left msg ->
          D.fail (E.BadConstraint msg)


depsDecoder :: Decoder a -> Decoder (Map Name a)
depsDecoder valueDecoder =
  Map.fromList <$> (
    traverse validateKey =<< D.pairs valueDecoder
  )


validateKey :: (Text, a) -> Decoder (Name, a)
validateKey (key, value) =
  case Pkg.fromText key of
    Right name ->
      D.succeed (name, value)

    Left _ ->
      D.fail (E.BadDependencyName key)


dirDecoder :: Decoder FilePath
dirDecoder =
  do  maybeText <- D.maybe D.text
      case maybeText of
        Nothing ->
          D.fail E.BadDirectoryNotString

        Just txt ->
          D.succeed (Text.unpack txt)



-- EXPOSED MODULES DECODER


exposedDecoder :: Decoder Exposed
exposedDecoder =
  D.oneOf
    [ ExposedList <$> D.list moduleDecoder
    , do  pairs <- D.pairs (D.list moduleDecoder)
          traverse_ (checkHeader . fst) pairs
          return (ExposedDict pairs)
    ]


moduleDecoder :: Decoder Module.Raw
moduleDecoder =
  D.mapError E.BadModuleName Module.decoder


checkHeader :: Text -> Decoder ()
checkHeader header =
  if Text.length header < 20 then
    D.succeed ()
  else
    D.fail (E.BadModuleHeaderTooLong header)
