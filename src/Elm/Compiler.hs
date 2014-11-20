{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
module Elm.Compiler
    ( version
    , parseDependencies, compile
    , runtimeDebugPath
    ) where

import Control.Monad.Error (MonadError, throwError)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Text.PrettyPrint as P

import qualified AST.Module as Module (HeaderAndImports(HeaderAndImports), toInterface)
import qualified Compile
import qualified Elm.Compiler.Module as PublicModule
import qualified Elm.Compiler.Version as Version
import Elm.Utils ((|>))
import qualified Elm.Utils as Utils
import qualified Generate.JavaScript as JS
import qualified Parse.Helpers as Help
import qualified Parse.Module as Parse
import qualified Paths_elm_compiler as Paths


-- VERSION

version :: String
version =
    Version.version


-- DEPENDENCIES

parseDependencies
    :: (MonadError String m)
    => String
    -> m (PublicModule.Name, [PublicModule.Name])
parseDependencies src =
    case Help.iParse Parse.headerAndImports src of
        Left msg ->
            throwError (show msg)

        Right (Module.HeaderAndImports names _exports imports) ->
            return
                ( PublicModule.Name names
                , map (PublicModule.Name . fst) imports
                )


-- COMPILATION

{-| Compiles Elm source code to JavaScript. -}
compile
    :: String
    -> String
    -> String
    -> Map.Map PublicModule.Name PublicModule.Interface
    -> Either String (PublicModule.Interface, String)
compile user packageName source interfaces =
  let unwrappedInterfaces =
        Map.mapKeysMonotonic (\(PublicModule.Name name) -> name) interfaces
  in
      case Compile.compile user packageName unwrappedInterfaces source of
        Right modul ->
            Right (Module.toInterface modul, JS.generate modul)

        Left docs ->
            map P.render docs
              |> List.intersperse ""
              |> unlines
              |> Left


-- DATA FILES

{-| Path to the debugger runtime.
-}
runtimeDebugPath :: IO FilePath
runtimeDebugPath =
    Utils.getAsset "compiler" Paths.getDataFileName "runtime/debug.js"
