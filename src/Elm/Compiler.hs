{-# OPTIONS_GHC -Wall #-}
module Elm.Compiler
    ( version
    , parseDependencies, Tag(..)
    , compile, Context(..), Result(..)
    , Localizer, dummyLocalizer
    , Error, errorToString, errorToJson, printError
    , Warning, warningToString, warningToJson, printWarning
    )
    where

import qualified Data.Aeson as Json
import qualified Data.Map as Map
import qualified Data.Text.Lazy as LazyText
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
import qualified Reporting.Bag as Bag
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


data Tag
  = Normal
  | Effect
  | Port


parseDependencies :: String -> Either [Error] (Tag, PublicModule.Raw, [PublicModule.Raw])
parseDependencies sourceCode =
  let
    (Result.Result _ _ answer) =
      Parse.parse sourceCode Parse.header

    getDeps (Module.Header sourceTag name _ _ _ imports) =
      let
        tag =
          case sourceTag of
            Module.Normal -> Normal
            Module.Port _ -> Port
            Module.Effect _ -> Effect
      in
        ( tag
        , name
        , map (fst . A.drop) imports
        )
  in
    Result.answerToEither (Error . A.map Error.Syntax) getDeps answer



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

    (Result.Result oneLocalizer warnings answer) =
      do  modul <- Compile.compile packageName dependencies interfaces source
          docs <- Result.format Error.Docs (docsGen isExposed modul)

          let interface = Module.toInterface packageName modul
          let javascript = JS.generate modul

          return (Result docs interface javascript)
  in
    ( Result.oneToValue dummyLocalizer Localizer oneLocalizer
    , Bag.toList Warning warnings
    , Result.answerToEither Error id answer
    )


data Context = Context
    { _packageName :: Package.Name
    , _isExposed :: Bool
    , _dependencies :: [PublicModule.Canonical]
    }


data Result = Result
    { _docs :: Maybe Docs.Documentation
    , _interface :: PublicModule.Interface
    , _js :: LazyText.Text
    }


docsGen :: Bool -> Module.Optimized -> Docs.Result w (Maybe Docs.Documentation)
docsGen isExposed (Module.Module name _ info) =
  if not isExposed then
    Result.ok Nothing

  else
    let
      toDocs checked =
        Just (Docs.fromCheckedDocs (ModuleName._module name) checked)
    in
      toDocs <$> Docs.check (Module.exports info) (Module.docs info)



-- DEALIASER


newtype Localizer =
    Localizer RenderType.Localizer


dummyLocalizer :: Localizer
dummyLocalizer =
    Localizer Map.empty



-- ERRORS


newtype Error =
    Error (A.Located Error.Error)


errorToString :: Bool -> Localizer -> String -> String -> Error -> String
errorToString isEmacsStyle (Localizer localizer) location source (Error (A.A region err)) =
    Report.toString location isEmacsStyle region (Error.toReport localizer err) source


printError :: Handle -> Bool -> Localizer -> String -> String -> Error -> IO ()
printError handle isEmacsStyle (Localizer localizer) location source (Error (A.A region err)) =
    Report.toHandle handle isEmacsStyle location region (Error.toReport localizer err) source


errorToJson :: Localizer -> String -> Error -> Json.Value
errorToJson (Localizer localizer) location (Error err) =
    Error.toJson localizer location err



-- WARNINGS


newtype Warning =
    Warning (A.Located Warning.Warning)


warningToString :: Bool -> Localizer -> String -> String -> Warning -> String
warningToString isEmacsStyle (Localizer localizer) location source (Warning (A.A region wrn)) =
    Report.toString location isEmacsStyle region (Warning.toReport localizer wrn) source


printWarning :: Handle -> Bool -> Localizer -> String -> String -> Warning -> IO ()
printWarning handle isEmacsStyle (Localizer localizer) location source (Warning (A.A region wrn)) =
    Report.toHandle handle isEmacsStyle location region (Warning.toReport localizer wrn) source


warningToJson :: Localizer -> String -> Warning -> Json.Value
warningToJson (Localizer localizer) location (Warning wrn) =
    Warning.toJson localizer location wrn


