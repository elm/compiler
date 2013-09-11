{- | This module exports the functions necessary for compiling Elm code into the
     respective HTML, CSS, and JS code.

     The documentation for the Elm language is available at
     <http://elm-lang.org/Documentation.elm>, and many interactive examples are
     available at <http://elm-lang.org/Examples.elm>
-}
module Language.Elm (compile, moduleName, runtime, docs) where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Version (showVersion)
import Generate.JavaScript (jsModule)
import Generate.Html (generateHtml)
import Initialize (buildFromSource)
import Parse.Module (getModuleName)
import SourceSyntax.Module
import Text.Blaze.Html (Html)
import qualified Text.PrettyPrint as P
import qualified Metadata.Prelude as Prelude
import Paths_Elm

-- |This function compiles Elm code to JavaScript. It will return either
--  an error message or the compiled JS code.
compile :: String -> Either String String
compile source =
    case buildFromSource False Prelude.interfaces source of
      Left docs -> Left . unlines . List.intersperse "" $ map P.render docs
      Right modul -> Right $ jsModule (modul :: MetadataModule () ())

-- |This function extracts the module name of a given source program.
moduleName :: String -> Maybe String
moduleName = getModuleName

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
