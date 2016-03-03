{-# OPTIONS_GHC -Wall #-}
module Nitpick.TopLevelTypes (topLevelTypes) where

import Data.Map ((!))
import Prelude hiding (maybe)
import qualified Data.Foldable as F
import qualified Data.Map as Map

import qualified AST.Expression.Valid as Valid
import qualified AST.Declaration as Decl
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Elm.Package as Pkg
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



topLevelTypes
    :: Map.Map String Type.Canonical
    -> [Decl.Valid]
    -> Result.Result Warning.Warning Error.Error ()
topLevelTypes typeEnv validDecls =
  do  F.traverse_ (warnMissingAnnotation typeEnv) validDecls
      checkMainType typeEnv validDecls



-- MISSING ANNOTATIONS


warnMissingAnnotation
    :: Map.Map String Type.Canonical
    -> Decl.Valid
    -> Result.Result Warning.Warning Error.Error ()
warnMissingAnnotation typeEnv (A.A (region,_) decl) =
  case decl of
    Decl.Def (Valid.Definition (A.A _ (P.Var name)) _ Nothing) ->
      Result.warn region (Warning.MissingTypeAnnotation name (typeEnv ! name))

    _ ->
      return ()



-- MAIN TYPE


checkMainType
    :: Map.Map String Type.Canonical
    -> [Decl.Valid]
    -> Result.Result w Error.Error ()
checkMainType typeEnv decls =
    case decls of
      A.A (region,_) (Decl.Def (Valid.Definition (A.A _ (P.Var "main")) _ _)) : _ ->
          case Map.lookup "main" typeEnv of
            Nothing ->
              return ()

            Just tipe ->
              case Type.deepDealias tipe of
                Type.App (Type.Type name) [_] | name == virtualDomNode ->
                  return ()

                Type.App (Type.Type name) [_] | name == program ->
                  return ()

                _ ->
                  Result.throw region (Error.BadMain tipe)

      _ : remainingDecls ->
          checkMainType typeEnv remainingDecls

      [] ->
          return ()


virtualDomNode :: Var.Canonical
virtualDomNode =
  Var.fromModule
    (ModuleName.Canonical (Pkg.Name "evancz" "virtual-dom") ["VirtualDom"])
    "Node"


program :: Var.Canonical
program =
  Var.fromModule (ModuleName.inCore ["Platform"]) "Program"

