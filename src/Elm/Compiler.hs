{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
module Elm.Compiler
    ( version, rawVersion
    , parseDependencies, compile
    ) where

import Control.Monad.Error.Class (MonadError, throwError)
import qualified Data.Map as Map

import qualified AST.Module as Module
import qualified Compile
import qualified Elm.Compiler.Module as PublicModule
import qualified Elm.Compiler.Version as Version
import qualified Generate.JavaScript as JS
import qualified Parse.Helpers as Help
import qualified Parse.Module as Parse
import qualified Reporting.Annotation as A
import qualified Reporting.Task as Task


-- VERSION

version :: String
version =
    Version.version


rawVersion :: [Int]
rawVersion =
    Version.rawVersion


-- DEPENDENCIES

parseDependencies
    :: (MonadError String m)
    => String
    -> m (PublicModule.Name, [PublicModule.Name])
parseDependencies src =
    case Help.iParse Parse.header src of
        Left msg ->
            throwError (show msg)

        Right (Module.Header names _exports imports) ->
            return
                ( PublicModule.Name names
                , map (PublicModule.Name . fst . A.drop) imports
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

      task =
        Compile.compile user packageName unwrappedInterfaces source
  in
      case Task.run task of
        (Right modul, _warnings) ->
            Right (Module.toInterface modul, JS.generate modul)

        (Left _docs, _warnings) ->
            Left [error "TODO:compile"]
