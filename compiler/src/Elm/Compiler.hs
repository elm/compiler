{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler
  ( version
  , Compile.DocsFlag(..)
  , Compile.Artifacts(..)
  , compile
  , Error.Error
  , errorsToDoc
  , errorsToJson
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
import qualified Json.Encode as Encode
import qualified Reporting.Doc as D
import qualified Reporting.Error as Error
import qualified Reporting.Render.Code as Code
import qualified Reporting.Region as Region
import qualified Reporting.Report as Report
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



-- VERSION


version :: Pkg.Version
version =
  Elm.Compiler.Version.version



-- COMPILE


compile
  :: Compile.DocsFlag
  -> Pkg.Name
  -> Map.Map M.Raw M.Canonical
  -> M.Interfaces
  -> BS.ByteString
  -> ( [Warning.Warning], Either [Error.Error] Compile.Artifacts )
compile docsFlag pkg importDict interfaces source =
  Result.run $ Compile.compile docsFlag pkg importDict interfaces source



-- ERRORS TO DOC


errorsToDoc :: FilePath -> Text.Text -> [Error.Error] -> D.Doc
errorsToDoc filePath source errors =
  let
    reports =
      concatMap (Error.toReports (Code.toSource source)) errors
  in
  D.vcat $ map (Report.toDoc filePath) reports



-- ERRORS TO JSON


errorsToJson :: M.Raw -> FilePath -> Text.Text -> [Error.Error] -> Encode.Value
errorsToJson moduleName filePath source errors =
  let
    reports =
      concatMap (Error.toReports (Code.toSource source)) errors
  in
  Encode.object
    [ ("path", Encode.text (Text.pack filePath))
    , ("name", Encode.name moduleName)
    , ("problems", Encode.array (map reportToJson reports))
    ]


reportToJson :: Report.Report -> Encode.Value
reportToJson (Report.Report title region _sgstns message) =
  Encode.object
    [ ("title", Encode.text (Text.pack title))
    , ("region", Region.encode region)
    , ("message", D.encode message)
    ]