{- | This module re-exports the modules necessary for compiling Elm code into the
     respective HTML, JS and CSS code.

     It also provides a predefined generateHtml function for use with the Blaze markup library
     as well as a simple QuasiQuoter for embedding literal elm code in a Haskell file.

     The documentation for the Elm language is available at <http://elm-lang.org/Documentation.elm>
-}
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
import Language.Elm.Quasi
