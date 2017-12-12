{-# OPTIONS_GHC -Wall #-}
module Elm.Compiler
  ( version
  , Context(..)
  , Compile.Artifacts(..)
  , compile
  , Localizer
  , dummyLocalizer
  , Error.Error
  , errorToDoc
  , errorToJson
  , Warning.Warning
  , warningToDoc
  , warningToJson
  )
  where

import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Text.PrettyPrint.ANSI.Leijen (Doc)

import qualified Compile
import qualified Elm.Compiler.Module as M
import qualified Elm.Compiler.Version
import qualified Elm.Package as Pkg
import qualified Reporting.Error as Error
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



-- VERSION


version :: Pkg.Version
version =
  Elm.Compiler.Version.version



-- COMPILE


data Context =
  Context
    { _package :: Pkg.Name
    , _exposed :: Bool
    , _imports :: Map.Map M.Raw M.Canonical
    , _interfaces :: M.Interfaces
    }


compile :: Context -> BS.ByteString -> (Localizer, [Warning.Warning], Either [Error.Error] Compile.Artifacts)
compile (Context pkg exposed importDict interfaces) source =
  let
    docsFlag =
      if exposed then Compile.YesDocs else Compile.NoDocs

    (warnings, errorOrArtifacts) =
      Result.run $ Compile.compile docsFlag pkg importDict interfaces source
  in
    ( dummyLocalizer, warnings, errorOrArtifacts )



-- DEALIASER


newtype Localizer =
  Localizer RenderType.Localizer


dummyLocalizer :: Localizer
dummyLocalizer =
  Localizer Map.empty



-- ERRORS


errorToDoc :: Localizer -> String -> String -> Error.Error -> Doc
errorToDoc (Localizer localizer) location source err =
  error "TODO Report.toDoc" location (Error.toReport localizer err) source


errorToJson :: Localizer -> String -> Error.Error -> Json.Value
errorToJson (Localizer localizer) location err =
  error "TODO Error.toJson" localizer location err



-- WARNINGS


warningToDoc :: Localizer -> String -> String -> Warning.Warning -> Doc
warningToDoc (Localizer localizer) location source warning =
  error "TODO Report.toDoc" location (Warning.toReport localizer warning) source


warningToJson :: Localizer -> String -> Warning.Warning -> Json.Value
warningToJson (Localizer localizer) location warning =
  error "TODO Warning.toJson" localizer location warning
