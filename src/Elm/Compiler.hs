{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
module Elm.Compiler
    ( version, rawVersion
    , parseDependencies, compile
    , Error, errorToString, errorToJson
    , Warning, warningToString, warningToJson
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
import qualified Reporting.Error as Error
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning


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
    -> ([Warning], Either [Error] (PublicModule.Interface, String))
compile user packageName source interfaces =
  let unwrappedInterfaces =
          Map.mapKeysMonotonic (\(PublicModule.Name name) -> name) interfaces

      (Result.Result warnings rawResult) =
          Compile.compile user packageName unwrappedInterfaces source
  in
      (,) (map Warning warnings) $
      case rawResult of
        Result.Ok modul ->
            Right (Module.toInterface modul, JS.generate modul)

        Result.Err errors ->
            Left (map Error errors)


-- ERRORS

newtype Error = Error (A.Located Error.Error)


errorToString :: Error -> String
errorToString (Error err) =
    Error.toString err


errorToJson :: Error -> String
errorToJson (Error err) =
    Error.toJson err


-- WARNINGS

newtype Warning = Warning (A.Located Warning.Warning)


warningToString :: Warning -> String
warningToString (Warning warning) =
    Warning.toString warning


warningToJson :: Warning -> String
warningToJson (Warning warning) =
    Warning.toJson warning


