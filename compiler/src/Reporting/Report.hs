{-# LANGUAGE OverloadedStrings #-}
module Reporting.Report
    ( Report(..)
    , toDoc
    , toCodeSnippet
    , toCodePair
    )
    where


import qualified Reporting.Doc as D
import qualified Reporting.Region as R
import qualified Reporting.Render.Code as Code



-- BUILD REPORTS


data Report =
  Report
    { _title :: String
    , _region :: R.Region
    , _sgstns :: [String]
    , _message :: D.Doc
    }


toDoc :: FilePath -> Report -> D.Doc
toDoc filePath (Report title _ _ message) =
  D.vcat
    [ messageBar title filePath
    , ""
    , message
    , ""
    ]


messageBar :: String -> FilePath -> D.Doc
messageBar title filePath =
  let
    usedSpace =
      4 + length title + 1 + length filePath
  in
    D.dullcyan $ D.fromString $
      "-- " ++ title
      ++ " " ++ replicate (max 1 (80 - usedSpace)) '-'
      ++ " " ++ filePath



-- CODE FORMATTING


toCodeSnippet :: Code.Source -> R.Region -> Maybe R.Region -> (D.Doc, D.Doc) -> D.Doc
toCodeSnippet source region highlight (preHint, postHint) =
  D.vcat
    [ preHint
    , ""
    , Code.render source region highlight
    , postHint
    ]


toCodePair :: Code.Source -> R.Region -> R.Region -> (D.Doc, D.Doc) -> (D.Doc, D.Doc, D.Doc) -> D.Doc
toCodePair source r1 r2 (oneStart, oneEnd) (twoStart, twoMiddle, twoEnd) =
  case Code.renderPair source r1 r2 of
    Code.OneLine codeDocs ->
      D.vcat
        [ oneStart
        , ""
        , codeDocs
        , oneEnd
        ]

    Code.TwoChunks code1 code2 ->
      D.vcat
        [ twoStart
        , ""
        , code1
        , twoMiddle
        , ""
        , code2
        , twoEnd
        ]
