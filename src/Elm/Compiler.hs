{-# OPTIONS_GHC -Wall #-}
module Elm.Compiler
  ( version
  , Context(..)
  , Compile.Artifacts(..)
  , compile
  , Localizer
  , dummyLocalizer
  , Error.Error
  , errorsToDoc
  , Warning.Warning
  )
  where


import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Compile
import qualified Elm.Compiler.Module as M
import qualified Elm.Compiler.Version
import qualified Elm.Package as Pkg
import qualified Reporting.Error as Error
import qualified Reporting.Helpers as H
import qualified Reporting.Render.Code as Code
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Report as Report
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


errorsToDoc :: FilePath -> Text.Text -> Localizer -> [Error.Error] -> H.Doc
errorsToDoc filePath source (Localizer localizer) errors =
  H.vcat $
    map
      (Report.toDoc filePath)
      (concatMap (Error.toReports (Code.toSource source) localizer) errors)
