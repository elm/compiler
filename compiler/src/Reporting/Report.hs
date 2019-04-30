{-# LANGUAGE OverloadedStrings #-}
module Reporting.Report
    ( Report(..)
    , toCodeSnippet
    , toCodePair
    )
    where


import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import qualified Reporting.Render.Code as Code



-- BUILD REPORTS


data Report =
  Report
    { _title :: String
    , _region :: A.Region
    , _sgstns :: [String]
    , _message :: D.Doc
    }



-- CODE FORMATTING


toCodeSnippet :: Code.Source -> A.Region -> Maybe A.Region -> (D.Doc, D.Doc) -> D.Doc
toCodeSnippet source region highlight (preHint, postHint) =
  D.vcat
    [ preHint
    , ""
    , Code.render source region highlight
    , postHint
    ]


toCodePair :: Code.Source -> A.Region -> A.Region -> (D.Doc, D.Doc) -> (D.Doc, D.Doc, D.Doc) -> D.Doc
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
