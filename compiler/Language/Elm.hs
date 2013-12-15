{- | This module exports functions for compiling Elm to JS
     and some utilities for making it easier to find some Elm-related files.

     Learning resources for Elm are available at
     <http://elm-lang.org.elm/Learn.elm> and many interactive examples are
     available at <http://elm-lang.org/Examples.elm>
-}
module Language.Elm (compile, moduleName, runtime, docs) where

import qualified Data.List as List
import qualified Generate.JavaScript as JS
import qualified Build.Source as Source
import qualified Parse.Module as Parser
import qualified SourceSyntax.Module as M
import qualified Text.PrettyPrint as P
import qualified Metadata.Prelude as Prelude
import qualified Paths_Elm as This
import System.IO.Unsafe

-- |This function compiles Elm code to JavaScript. It will return either
--  an error message or the compiled JS code.
compile :: String -> Either String String
compile source =
    case Source.build False interfaces source of
      Left docs -> Left . unlines . List.intersperse "" $ map P.render docs
      Right modul -> Right $ JS.generate modul

{-# NOINLINE interfaces #-}
interfaces :: M.Interfaces
interfaces = unsafePerformIO $ Prelude.interfaces

-- |This function extracts the module name of a given source program.
moduleName :: String -> Maybe String
moduleName = Parser.getModuleName

-- |The absolute path to Elm's runtime system.
runtime :: IO FilePath
runtime = This.getDataFileName "elm-runtime.js"

-- |The absolute path to Elm's core library documentation.
docs :: IO FilePath
docs = This.getDataFileName "docs.json"
