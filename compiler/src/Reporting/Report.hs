{-# LANGUAGE OverloadedStrings #-}
module Reporting.Report
    ( Report(..)
    )
    where


import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D



-- BUILD REPORTS


data Report =
  Report
    { _title :: String
    , _region :: A.Region
    , _sgstns :: [String]
    , _message :: D.Doc
    }
