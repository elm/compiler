{-# OPTIONS_GHC -Wall #-}
module Elm.Compiler
  ( version
  , Context(..)
  , Compile.Artifacts(..)
  , compile
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



-- ERRORS


errorsToDoc :: FilePath -> Text.Text -> [Error.Error] -> H.Doc
errorsToDoc filePath source errors =
  H.vcat $
    map
      (Report.toDoc filePath)
      (concatMap (Error.toReports (Code.toSource source) Map.empty) errors)
