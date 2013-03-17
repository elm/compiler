{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{- | This module exports the functions necessary for compiling Elm code into the
     respective HTML, JS and CSS code.

     The type class @'ElmSource'@ requires an instance for all types that the Elm
     compiler understands. The provided instances for String, Text and QuasiQuoted
     Elm source code should be sufficient.

     The documentation for the Elm language is available at
     <http://elm-lang.org/Documentation.elm>, and many interactive examples are
     available at <http://elm-lang.org/Examples.elm>

     Example implementations using Yesod and Happstack are available
     at <https://github.com/tazjin/Elm/tree/master/Examples>
-}
module Language.Elm (
    compile, toHtml,
    runtimeLocation
    ) where

import Data.Version (showVersion)
import CompileToJS
import ExtractNoscript
import GenerateHtml
import Initialize
import Text.Blaze.Html (Html)
import Language.Elm.Quasi
import Paths_Elm

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

-- |This function compiles Elm code to three separate parts: HTML, CSS,
--  and JavaScript. The HTML is only the contents of the body, so the three
--  parts must be combined in a basic HTML skeleton.
compile :: String -> String
compile source = either showErr jsModule modul
  where modul = buildFromSource source

-- |This function compiles Elm code into a full HTML page.
toHtml :: String -- ^ Location of elm-min.js as expected by the browser
       -> String -- ^ The page title
       -> String -- ^ The elm source code
       -> Html
toHtml = generateHtml

-- |The absolute path to Elm's runtime system.
runtimeLocation :: IO FilePath
runtimeLocation = getDataFileName "elm-runtime.js"

