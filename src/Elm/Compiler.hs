{-# OPTIONS_GHC -Wall #-}
module Elm.Compiler
    ( version
    , parseDependencies
    , compile, Context(..), Result(..)
    , Localizer, dummyLocalizer
    , Error, errorToString, errorToJson, printError
    , Warning, warningToString, warningToJson, printWarning
    )
    where

import qualified Data.Aeson as Json
import qualified Data.Map as Map
import System.IO (Handle)

import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified Compile
import qualified Docs.Check as Docs
import qualified Elm.Compiler.Version
import qualified Elm.Compiler.Module as PublicModule
import qualified Elm.Docs as Docs
import qualified Elm.Package as Package
import qualified Generate.JavaScript as JS
import qualified Parse.Module as Parse
import qualified Parse.Parse as Parse
import qualified Reporting.Annotation as A
import qualified Reporting.Error as Error
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Report as Report
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning


-- VERSION

version :: Package.Version
version =
  Elm.Compiler.Version.version


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

      Result.Ok (Module.Header name _docs _exports imports) ->
          Right
            ( PublicModule.Name name
            , map (PublicModule.Name . fst . A.drop) imports
            )


-- COMPILATION

{-| Compiles Elm source code to JavaScript. -}
compile
    :: Context
    -> String
    -> PublicModule.Interfaces
    -> (Localizer, [Warning], Either [Error] Result)

compile context source interfaces =
  let
    (Context packageName isExposed dependencies) =
      context

    (Result.Result (localizer, warnings) rawResult) =
      do  modul <- Compile.compile packageName dependencies interfaces source
          docs <- docsGen isExposed modul

          let interface = Module.toInterface packageName modul
          let javascript = JS.generate modul

          return (Result docs interface javascript)
  in
    ( maybe dummyLocalizer Localizer localizer
    , map Warning warnings
    , Result.destruct (Left . map Error) Right rawResult
    )


data Context = Context
    { _packageName :: Package.Name
    , _isExposed :: Bool
    , _dependencies :: [PublicModule.CanonicalName]
    }


data Result = Result
    { _docs :: Maybe Docs.Documentation
    , _interface :: PublicModule.Interface
    , _js :: String
    }


docsGen
    :: Bool
    -> Module.Optimized
    -> Result.Result w Error.Error (Maybe Docs.Documentation)
docsGen isExposed modul =
  if not isExposed then
    Result.ok Nothing
  else
    let
      getChecked =
        Docs.check (Module.exports modul) (Module.docs modul)

      name =
        PublicModule.Name (ModuleName._module (Module.name modul))

      toDocs checked =
        Docs.fromCheckedDocs name checked
    in
      (Just . toDocs) `fmap` Result.mapError Error.Docs getChecked


-- DEALIASER

newtype Localizer =
    Localizer RenderType.Localizer


dummyLocalizer :: Localizer
dummyLocalizer =
    Localizer Map.empty


-- ERRORS

newtype Error =
    Error (A.Located Error.Error)


errorToString :: Localizer -> String -> String -> Error -> String
errorToString (Localizer localizer) location source (Error (A.A region err)) =
    Report.toString location region (Error.toReport localizer err) source


printError :: Handle -> Localizer -> String -> String -> Error -> IO ()
printError handle (Localizer localizer) location source (Error (A.A region err)) =
    Report.toHandle handle location region (Error.toReport localizer err) source


errorToJson :: Localizer -> String -> Error -> Json.Value
errorToJson (Localizer localizer) location (Error err) =
    Error.toJson localizer location err


-- WARNINGS

newtype Warning =
    Warning (A.Located Warning.Warning)


warningToString :: Localizer -> String -> String -> Warning -> String
warningToString (Localizer localizer) location source (Warning (A.A region wrn)) =
    Report.toString location region (Warning.toReport localizer wrn) source


printWarning :: Handle -> Localizer -> String -> String -> Warning -> IO ()
printWarning handle (Localizer localizer) location source (Warning (A.A region wrn)) =
    Report.toHandle handle location region (Warning.toReport localizer wrn) source


warningToJson :: Localizer -> String -> Warning -> Json.Value
warningToJson (Localizer localizer) location (Warning wrn) =
    Warning.toJson localizer location wrn


