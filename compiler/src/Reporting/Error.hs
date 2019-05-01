{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error
  ( Module(..)
  , Error(..)
  , toDoc
  , toJson
  )
  where


import qualified Data.ByteString as B
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE
import qualified Data.OneOrMore as OneOrMore
import qualified Data.Utf8 as Utf8

import qualified Elm.ModuleName as ModuleName
import qualified File
import qualified Json.Encode as E
import Json.Encode ((==>))
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import qualified Reporting.Error.Canonicalize as Canonicalize
import qualified Reporting.Error.Import as Import
import qualified Reporting.Error.Main as Main
import qualified Reporting.Error.Pattern as Pattern
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Error.Type as Type
import qualified Reporting.Render.Code as Code
import qualified Reporting.Render.Type.Localizer as L
import qualified Reporting.Report as Report



-- MODULE


data Module =
  Module
    { _name :: ModuleName.Raw
    , _path :: FilePath
    , _time :: File.Time
    , _source :: B.ByteString
    , _error :: Error
    }



-- ERRORS


data Error
  = BadSyntax Syntax.Error
  | BadImports (NE.List Import.Error)
  | BadNames (OneOrMore.OneOrMore Canonicalize.Error)
  | BadTypes L.Localizer (NE.List Type.Error)
  | BadMains L.Localizer (OneOrMore.OneOrMore Main.Error)
  | BadPatterns (NE.List Pattern.Error)



-- TO REPORT


toReports :: Code.Source -> Error -> NE.List Report.Report
toReports source err =
  case err of
    BadSyntax syntaxError ->
      NE.List (Syntax.toReport source syntaxError) []

    BadImports errs ->
      fmap (Import.toReport source) errs

    BadNames errs ->
      fmap (Canonicalize.toReport source) (OneOrMore.destruct NE.List errs)

    BadTypes localizer errs ->
      fmap (Type.toReport source localizer) errs

    BadMains localizer errs ->
      fmap (Main.toReport localizer source) (OneOrMore.destruct NE.List errs)

    BadPatterns errs ->
      fmap (Pattern.toReport source) errs



-- TO DOC


toDoc :: NE.List Module -> D.Doc
toDoc modules =
  let
    timeCompare m1 m2 = compare (_time m1) (_time m2)
    (NE.List m ms)    = NE.sortBy timeCompare modules
  in
  D.vcat (toDocHelp m ms)


toDocHelp :: Module -> [Module] -> [D.Doc]
toDocHelp module1 modules =
  case modules of
    [] ->
      [moduleToDoc module1]

    module2 : otherModules ->
      moduleToDoc module1
      : toSeparator module1 module2
      : toDocHelp module2 otherModules


toSeparator :: Module -> Module -> D.Doc
toSeparator beforeModule afterModule =
  let
    before = ModuleName.toChars (_name beforeModule) ++ "  ↑    "
    after  = "    ↓  " ++  ModuleName.toChars (_name afterModule)
  in
    D.dullred $ D.vcat $
      [ D.indent (80 - length before) (D.fromChars before)
      , "====o======================================================================o===="
      , D.fromChars after
      , ""
      , ""
      ]



-- MODULE TO DOC


moduleToDoc :: Module -> D.Doc
moduleToDoc (Module _ path _ source err) =
  let
    reports =
      toReports (Code.toSource source) err
  in
  D.vcat $ map (reportToDoc path) (NE.toList reports)


reportToDoc :: FilePath -> Report.Report -> D.Doc
reportToDoc path (Report.Report title _ _ message) =
  D.vcat
    [ toMessageBar title path
    , ""
    , message
    , ""
    ]


toMessageBar :: String -> FilePath -> D.Doc
toMessageBar title filePath =
  let
    usedSpace =
      4 + length title + 1 + length filePath
  in
    D.dullcyan $ D.fromChars $
      "-- " ++ title
      ++ " " ++ replicate (max 1 (80 - usedSpace)) '-'
      ++ " " ++ filePath



-- TO JSON


toJson :: NE.List Module -> E.Value
toJson modules =
  E.array (map moduleToJson (NE.toList modules))


moduleToJson :: Module -> E.Value
moduleToJson (Module name path _ source err) =
  let
    reports =
      toReports (Code.toSource source) err
  in
  E.object
    [ "path" ==> E.string (Utf8.fromChars path)
    , "name" ==> E.string (Name.toUtf8 name)
    , "problems" ==> E.array (map reportToJson (NE.toList reports))
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
