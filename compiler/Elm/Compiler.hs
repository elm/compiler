{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
module Elm.Compiler (version, moduleNameToPath, moduleNameToString, parseDependencies, elmToJs) where

import Control.Monad.Error (MonadError, MonadIO, liftIO, throwError)
import qualified Data.List as List
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import qualified AST.Module as Module
import qualified Elm.Compiler.Version as Version
import qualified Metadata.Prelude as Prelude
import qualified Parse.Helpers as Help
import qualified Parse.Module as Parse


-- VERSION

version :: String
version =
    Version.version


-- MODULE NAMES

newtype ModuleName =
    ModuleName [String]


moduleNameToPath :: ModuleName -> FilePath
moduleNameToPath (ModuleName names) =
    List.foldl1 (</>) names


moduleNameToString :: ModuleName -> String
moduleNameToString (ModuleName names) =
    List.intercalate "." names


-- DEPENDENCIES

parseDependencies :: (MonadIO m, MonadError String m) => FilePath -> m (ModuleName, [ModuleName])
parseDependencies filePath =
    do  src <- liftIO (readFile filePath)
        case Help.iParse Parse.headerAndImports src of
            Left msg ->
                throwError (show msg)

            Right (Module.HeaderAndImports names _exports imports) ->
                return ( ModuleName names, map (ModuleName . fst) imports )


-- COMPILATION

{-| Compiles Elm source code to JavaScript. -}
elmToJs :: String -> Either String String
elmToJs source =
    error "elmToJs is not working yet"
{-
    case Source.build False interfaces source of
      Left docs -> Left . unlines . List.intersperse "" $ map P.render docs
      Right modul -> Right $ JS.generate modul
-}

{-# NOINLINE interfaces #-}
interfaces :: Module.Interfaces
interfaces =
    error "unsafePerformIO $ Prelude.interfaces False"

