{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler
  ( version
  , Context(..)
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
import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Compile
import qualified Elm.Compiler.Module as M
import qualified Elm.Compiler.Version
import qualified Elm.Package as Pkg
import qualified Json.Encode as Encode
import qualified Reporting.Error as Error
import qualified Reporting.Helpers as H
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


data Context =
  Context
    { _package :: Pkg.Name
    , _exposed :: Bool
    , _imports :: Map.Map M.Raw M.Canonical
    , _interfaces :: M.Interfaces
    }


compile :: Context -> BS.ByteString -> ([Warning.Warning], Either [Error.Error] Compile.Artifacts)
compile (Context pkg exposed importDict interfaces) source =
  let
    docsFlag =
      if exposed then Compile.YesDocs else Compile.NoDocs
  in
  Result.run $ Compile.compile docsFlag pkg importDict interfaces source



-- ERRORS TO DOC


errorsToDoc :: FilePath -> Text.Text -> [Error.Error] -> H.Doc
errorsToDoc filePath source errors =
  let
    reports =
      concatMap (Error.toReports (Code.toSource source) Map.empty) errors
  in
  H.vcat $ map (Report.toDoc filePath) reports



-- ERRORS TO JSON


errorsToJson :: FilePath -> Text.Text -> [Error.Error] -> Encode.Value
errorsToJson filePath source errors =
  let
    reports =
      concatMap (Error.toReports (Code.toSource source) Map.empty) errors
  in
  Encode.object
    [ ("path", Encode.text (Text.pack filePath))
    , ("errors", Encode.array (map reportToJson reports))
    ]


reportToJson :: Report.Report -> Encode.Value
reportToJson (Report.Report title region _sgstns message) =
  let
    messageString =
      P.displayS (P.renderPretty 1 80 message) ""
  in
  Encode.object
    [ ("title", Encode.text (Text.pack title))
    , ("region", Region.encode region)
    , ("message", Encode.text (Text.pack messageString))
    ]