{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Language.Elm (
    module Language.Elm.Initialize,
    module Language.Elm.CompileToJS,
    module Language.Elm.ExtractNoscript,
    generateHtml,
    elm,
    elmFile) where

import Language.Elm.CompileToJS
import Language.Elm.ExtractNoscript
import Language.Elm.Initialize
import Language.Elm.GenerateHtml
import Language.Haskell.TH
import Language.Haskell.TH.Quote

elm :: QuasiQuoter
elm = QuasiQuoter {quoteExp = \s -> [|s|] }

elmFile :: QuasiQuoter
elmFile = quoteFile elm