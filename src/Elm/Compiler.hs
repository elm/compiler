{-# OPTIONS_GHC -Wall #-}
module Elm.Compiler
  ( version
  , compile, Context(..), Result(..)
  , generate, JS.Target(..)
  , Localizer, dummyLocalizer
  , I.Error, errorToDoc, errorToJson
  , I.Warning, warningToDoc, warningToJson
  )
  where

import qualified Data.Aeson as Json
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.ByteString.Builder as BS
import Text.PrettyPrint.ANSI.Leijen (Doc)

import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified Compile
import qualified Docs.Check as Docs
import qualified Elm.Compiler.Internals as I
import qualified Elm.Compiler.Module as M
import qualified Elm.Compiler.Objects.Internal as Obj
import qualified Elm.Compiler.Version
import qualified Elm.Docs as Docs
import qualified Elm.Package as Package
import qualified Generate.JavaScript as JS
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



-- COMPILATION


{-| Compiles Elm source code to JavaScript. -}
compile :: Context -> Text -> (Localizer, [I.Warning], Either [I.Error] Result)
compile (Context packageName exposed importDict interfaces) source =
  let
    (Result.Result oneLocalizer warnings answer) =
      do  modul <- Compile.compile packageName importDict interfaces source
          docs <- Result.format id (docsGen exposed modul)

          let iface = Module.toInterface modul
          let objs = Obj.fromModule modul

          return (Result docs iface objs)
  in
    ( Result.oneToValue dummyLocalizer Localizer oneLocalizer
    , Bag.toList I.Warning warnings
    , Result.answerToEither I.Error id answer
    )


data Context =
  Context
    { _package :: Package.Name
    , _exposed :: Bool
    , _imports :: Map.Map M.Raw M.Canonical
    , _interfaces :: M.Interfaces
    }


data Result =
  Result
    { _docs :: Maybe Docs.Module
    , _iface :: M.Interface
    , _objs :: Obj.Graph
    }



-- DOCUMENTATION


docsGen :: Bool -> Module.Optimized -> Result.Result () w Error.Error (Maybe Docs.Module)
docsGen isExposed (Module.Module name info) =
  if not isExposed then
    Result.ok Nothing

  else
    let
      toDocs checked =
        Just (Docs.Module (ModuleName._module name) checked)
    in
      toDocs <$> Docs.check (Module.exports info) (Module.docs info)



-- CODE GENERATION


generate :: Bool -> JS.Target -> Obj.Graph -> Obj.Roots -> BS.Builder
generate =
  JS.generate



-- DEALIASER


newtype Localizer =
    Localizer RenderType.Localizer


dummyLocalizer :: Localizer
dummyLocalizer =
    Localizer Map.empty



-- ERRORS


errorToDoc :: Localizer -> String -> Text -> I.Error -> Doc
errorToDoc (Localizer localizer) location source (I.Error (A.A region err)) =
    Report.toDoc location region (Error.toReport localizer err) source


errorToJson :: Localizer -> String -> I.Error -> Json.Value
errorToJson (Localizer localizer) location (I.Error err) =
    Error.toJson localizer location err



-- WARNINGS


warningToDoc :: Localizer -> String -> Text -> I.Warning -> Doc
warningToDoc (Localizer localizer) location source (I.Warning (A.A region wrn)) =
    Report.toDoc location region (Warning.toReport localizer wrn) source


warningToJson :: Localizer -> String -> I.Warning -> Json.Value
warningToJson (Localizer localizer) location (I.Warning wrn) =
    Warning.toJson localizer location wrn
