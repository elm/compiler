{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler
  ( Compile.Artifacts(..)
  , compile
  , Error.Error
  , errorsToDoc
  , errorsToJson
  , Warning.Warning
  )
  where


import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Utf8 as Utf8

import qualified Compile
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Json.Encode as E
import Json.Encode ((==>))
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import qualified Reporting.Error as Error
import qualified Reporting.Render.Code as Code
import qualified Reporting.Report as Report
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



-- COMPILE


compile
  :: Pkg.Name
  -> Map.Map ModuleName.Raw ModuleName.Canonical
  -> I.Interfaces
  -> BS.ByteString
  -> ( [Warning.Warning], Either [Error.Error] Compile.Artifacts )
compile pkg importDict interfaces source =
  Result.run $ Compile.compile pkg importDict interfaces source



-- ERRORS TO DOC


errorsToDoc :: FilePath -> String -> [Error.Error] -> D.Doc
errorsToDoc filePath source errors =
  let
    reports =
      concatMap (Error.toReports (Code.toSource source)) errors
  in
  D.vcat $ map (Report.toDoc filePath) reports



-- ERRORS TO JSON


errorsToJson :: ModuleName.Raw -> FilePath -> String -> [Error.Error] -> E.Value
errorsToJson moduleName filePath source errors =
  let
    reports =
      concatMap (Error.toReports (Code.toSource source)) errors
  in
  E.object
    [ "path" ==> E.string (Utf8.fromChars filePath)
    , "name" ==> E.string (Name.toUtf8 moduleName)
    , "problems" ==> E.array (map reportToJson reports)
    ]


reportToJson :: Report.Report -> E.Value
reportToJson (Report.Report title region _sgstns message) =
  E.object
    [ "title" ==> E.string (Utf8.fromChars title)
    , "region" ==> encodeRegion region
    , "message" ==> D.encode message
    ]


encodeRegion :: A.Region -> E.Value
encodeRegion (A.Region (A.Position sr sc) (A.Position er ec)) =
  E.object
    [ "start" ==>
          E.object
            [ ("line", E.int (fromIntegral sr))
            , ("column", E.int (fromIntegral sc))
            ]
    , "end" ==>
          E.object
            [ ("line", E.int (fromIntegral er))
            , ("column", E.int (fromIntegral ec))
            ]
    ]
