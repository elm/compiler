{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
module Elm.Compiler (version, parseDependencies, compile) where

import Control.Monad.Error (MonadError, MonadIO, liftIO, throwError)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Text.PrettyPrint as P

import qualified AST.Module as Module (HeaderAndImports(HeaderAndImports), Interfaces, toInterface)
import qualified Build.Source as Source
import qualified Elm.Compiler.Module as PublicModule
import qualified Elm.Compiler.Version as Version
import qualified Generate.JavaScript as JS
import qualified Metadata.Prelude as Prelude
import qualified Parse.Helpers as Help
import qualified Parse.Module as Parse


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
    -> Map.Map PublicModule.Name PublicModule.Interface
    -> Either String PublicModule.Interface
compile source interfaces =
    case Source.build True unwrappedInterfaces source of
      Left docs -> Left . unlines . List.intersperse "" $ map P.render docs
      Right modul -> Right (Module.toInterface modul)
  where
    unwrappedInterfaces =
        Map.mapKeysMonotonic (\(PublicModule.Name name) -> name) interfaces
