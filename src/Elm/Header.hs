{-# OPTIONS_GHC -Wall #-}
module Elm.Header
  ( Tag(..)
  , parse
  )
  where


import Data.Text (Text)

import qualified AST.Module as Module
import qualified Elm.Compiler.Imports as Imports
import qualified Elm.Compiler.Internals as I
import qualified Elm.Compiler.Module as M
import qualified Elm.Package as Package
import qualified Parse.Helpers as Parse (run)
import qualified Parse.Module as Parse (header)
import qualified Reporting.Annotation as A
import qualified Reporting.Error as Error



-- HEADER TAGS


data Tag = Normal | Effect | Port



-- PARSE


parse :: Package.Name -> Text -> Either I.Error (Maybe (Tag, M.Raw), [M.Raw])
parse pkgName sourceCode =
  case Parse.run Parse.header sourceCode of
    Right header ->
      Right $ toSummary pkgName header

    Left err ->
      Left (I.Error (A.map Error.Syntax err))


toSummary :: Package.Name -> Module.Header [Module.UserImport] -> (Maybe (Tag, M.Raw), [M.Raw])
toSummary pkgName (Module.Header maybeHeaderDecl imports) =
  let
    dependencies =
      if pkgName == Package.core
        then map (A.drop . fst . A.drop) imports
        else map (A.drop . fst . A.drop) imports ++ map fst Imports.defaults
  in
    case maybeHeaderDecl of
      Nothing ->
        ( Nothing, dependencies )

      Just (Module.HeaderDecl sourceTag name _ _ _) ->
        let
          tag =
            case sourceTag of
              Module.Normal -> Normal
              Module.Port _ -> Port
              Module.Effect _ -> Effect
        in
          ( Just (tag, name), dependencies )
