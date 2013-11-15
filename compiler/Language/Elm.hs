{- | This module exports the functions necessary for compiling Elm to JS,
     and some utilities for making it easier to find some Elm-related files.

     The documentation for the Elm language is available at
     <http://elm-lang.org/Documentation.elm>, and many interactive examples are
     available at <http://elm-lang.org/Examples.elm>
-}
module Language.Elm ( compile
                    , interfaces
                    , moduleName
                    , runtime
                    , docs
                    , Interfaces
                    , ModuleInterface )
where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Version (showVersion)
import Generate.JavaScript (jsModule)
import Initialize (buildFromSource)
import Parse.Module (getModuleName)
import SourceSyntax.Module
import Text.Blaze.Html (Html)
import qualified Text.PrettyPrint as P
import qualified Metadata.Prelude as Prelude
import Paths_Elm

-- |This function compiles Elm code to JavaScript. It will return either
--  an error message or the compiled JS code.
compile :: Interfaces -> String -> Either String String
compile ifaces source = do
  case buildFromSource False ifaces source of
    Left docs -> Left . unlines . List.intersperse "" $ map P.render docs
    Right modul -> Right $ jsModule (modul :: MetadataModule () ())

interfaces :: IO Interfaces
interfaces = Prelude.interfaces

-- |This function extracts the module name of a given source program.
moduleName :: String -> Maybe String
moduleName = getModuleName

-- |The absolute path to Elm's runtime system.
runtime :: IO FilePath
runtime = getDataFileName "elm-runtime.js"

-- |The absolute path to Elm's core library documentation.
docs :: IO FilePath
docs = getDataFileName "docs.json"
