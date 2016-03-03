{-# OPTIONS_GHC -Wall #-}
module Nitpick.TopLevelTypes (topLevelTypes) where

import Control.Monad (foldM, when)
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified AST.Expression.Valid as Valid
import qualified AST.Declaration as Decl
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Elm.Package as Pkg
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



-- CHECK TOP LEVEL TYPES


topLevelTypes
  :: Map.Map String Type.Canonical
  -> [Decl.Valid]
  -> Result.Result Warning.Warning Error.Error ()
topLevelTypes typeEnv validDecls =
  do  maybeMainRegion <- foldM (warnMissingAnnotation typeEnv) Nothing validDecls
      maybe (return ()) (checkMainType typeEnv) maybeMainRegion



-- MISSING ANNOTATIONS


warnMissingAnnotation
  :: Map.Map String Type.Canonical
  -> Maybe R.Region
  -> Decl.Valid
  -> Result.Result Warning.Warning Error.Error (Maybe R.Region)
warnMissingAnnotation typeEnv maybeMainRegion (A.A (region,_) decl) =
  case decl of
    Decl.Def (Valid.Definition (A.A _ (P.Var name)) _ maybeType) ->
      do  when (Maybe.isNothing maybeType) $
            Result.warn region (Warning.MissingTypeAnnotation name (typeEnv ! name))

          return (if name == "main" then Just region else maybeMainRegion)

    _ ->
      return maybeMainRegion



-- CHECK MAIN TYPE


checkMainType :: Map.Map String Type.Canonical -> R.Region -> Result.Result w Error.Error ()
checkMainType typeEnv region =
  let
    tipe =
      typeEnv ! "main"
  in
    case Type.deepDealias tipe of
      Type.App name [_] | name `elem` [program, vdomNode] ->
        return ()

      _ ->
        Result.throw region (Error.BadMain tipe)


program :: Type.Canonical
program =
  Type.Type (Var.inCore ["Platform"] "Program")


vdomNode :: Type.Canonical
vdomNode =
  let
    vdom =
      ModuleName.Canonical (Pkg.Name "evancz" "virtual-dom") ["VirtualDom"]
  in
    Type.Type (Var.fromModule vdom "Node")

