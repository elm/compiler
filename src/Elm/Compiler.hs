{-# OPTIONS_GHC -Wall #-}
module Elm.Compiler
    ( version, rawVersion
    , parseDependencies
    , compile, Context(..), Result(..)
    , Dealiaser, dummyDealiaser
    , Error, errorToString, errorToJson, printError
    , Warning, warningToString, warningToJson, printWarning
    ) where

import qualified Data.Aeson as Json
import qualified Data.Map as Map

import qualified AST.Module as Module
import qualified Compile
import qualified Docs.Check as Docs
import qualified Elm.Compiler.Module as PublicModule
import qualified Elm.Compiler.Version as Version
import qualified Elm.Docs as Docs
import qualified Generate.JavaScript as JS
import qualified Parse.Module as Parse
import qualified Parse.Parse as Parse
import qualified Reporting.Annotation as A
import qualified Reporting.Error as Error
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning


-- VERSION

version :: String
version =
    Version.version


rawVersion :: [Int]
rawVersion =
    Version.rawVersion


-- DEPENDENCIES

parseDependencies
    :: String
    -> Either [Error] (PublicModule.Name, [PublicModule.Name])
parseDependencies sourceCode =
  let
    (Result.Result _warnings rawResult) =
      Parse.parse sourceCode Parse.header
  in
    case rawResult of
      Result.Err msgs ->
          Left $ map (Error . A.map Error.Syntax) msgs

      Result.Ok (Module.Header names _docs _exports imports) ->
          Right
            ( PublicModule.Name names
            , map (PublicModule.Name . fst . A.drop) imports
            )


-- COMPILATION

{-| Compiles Elm source code to JavaScript. -}
compile
    :: Context
    -> String
    -> Map.Map PublicModule.Name PublicModule.Interface
    -> (Dealiaser, [Warning], Either [Error] Result)

compile context source interfaces =
  let
    (Context user packageName isRoot isExposed) =
      context

    unwrappedInterfaces =
      Map.mapKeysMonotonic (\(PublicModule.Name name) -> name) interfaces

    (Result.Result (dealiaser, warnings) rawResult) =
      do  modul <- Compile.compile user packageName isRoot unwrappedInterfaces source
          docs <- docsGen isExposed modul
          return (Result docs (Module.toInterface modul) (JS.generate modul))
  in
    ( maybe dummyDealiaser Dealiaser dealiaser
    , map Warning warnings
    , Result.destruct (Left . map Error) Right rawResult
    )


data Context = Context
    { _user :: String
    , _packageName :: String
    , _isRoot :: Bool
    , _isExposed :: Bool
    }


data Result = Result
    { _docs :: Maybe Docs.Documentation
    , _interface :: PublicModule.Interface
    , _js :: String
    }


docsGen
    :: Bool
    -> Module.CanonicalModule
    -> Result.Result w Error.Error (Maybe Docs.Documentation)
docsGen isExposed modul =
  if not isExposed then
    Result.ok Nothing
  else
    let
      getChecked =
        Docs.check (Module.exports modul) (Module.docs modul)

      toDocs checked =
        Docs.fromCheckedDocs (PublicModule.Name (Module.names modul)) checked
    in
      (Just . toDocs) `fmap` Result.mapError Error.Docs getChecked


-- DEALIASER

newtype Dealiaser =
    Dealiaser P.Dealiaser


dummyDealiaser :: Dealiaser
dummyDealiaser =
    Dealiaser Map.empty


-- ERRORS

newtype Error =
    Error (A.Located Error.Error)


errorToString :: Dealiaser -> String -> String -> Error -> String
errorToString (Dealiaser dealiaser) location source (Error err) =
    Error.toString dealiaser location source err


printError :: Dealiaser -> String -> String -> Error -> IO ()
printError (Dealiaser dealiaser) location source (Error err) =
    Error.print dealiaser location source err


errorToJson :: Dealiaser -> String -> Error -> Json.Value
errorToJson (Dealiaser dealiaser) location (Error err) =
    Error.toJson dealiaser location err


-- WARNINGS

newtype Warning =
    Warning (A.Located Warning.Warning)


warningToString :: Dealiaser -> String -> String -> Warning -> String
warningToString (Dealiaser dealiaser) location source (Warning err) =
    Warning.toString dealiaser location source err


printWarning :: Dealiaser -> String -> String -> Warning -> IO ()
printWarning (Dealiaser dealiaser) location source (Warning err) =
    Warning.print dealiaser location source err


warningToJson :: Dealiaser -> String -> Warning -> Json.Value
warningToJson (Dealiaser dealiaser) location (Warning err) =
    Warning.toJson dealiaser location err


