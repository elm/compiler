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
    moduleName,
    runtime, docs
    ) where

import qualified Ast as Ast
import qualified Libraries as Libraries
import Data.List (intercalate)
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

-- |This function compiles Elm code to JavaScript.
compile :: String -> String
compile source = either showErr (jsModule . Libraries.addPrelude) modul
  where modul = buildFromSource source

-- |This function extracts the module name of a given source program.
moduleName :: String -> String
moduleName source = either (const "Main") getName modul
  where modul = buildFromSource source
        getName (Ast.Module names _ _ _) = intercalate "." names

-- |This function compiles Elm code into a full HTML page.
toHtml :: String -- ^ Location of elm-min.js as expected by the browser
       -> String -- ^ The page title
       -> String -- ^ The elm source code
       -> Html
toHtml = generateHtml

-- |The absolute path to Elm's runtime system.
runtime :: IO FilePath
runtime = getDataFileName "elm-runtime.js"

-- |The absolute path to Elm's core library documentation.
docs :: IO FilePath
docs = getDataFileName "docs.json"

