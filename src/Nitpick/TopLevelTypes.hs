{-# OPTIONS_GHC -Wall #-}
module Nitpick.TopLevelTypes (topLevelTypes) where

import qualified Data.Foldable as F
import Data.Map ((!))
import qualified Data.Map as Map

import qualified AST.Expression.Canonical as Can
import qualified AST.Expression.General as Expr
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Canonicalize.Effects as Effects
import qualified Elm.Package as Pkg
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



type Result =
  Result.Result () Warning.Warning Error.Error


type TypeDict =
  Map.Map String Type.Canonical



-- CHECK TOP LEVEL TYPES


topLevelTypes :: TypeDict -> Can.SortedDefs -> Result [Can.Def]
topLevelTypes typeEnv sortedDefs =
  let
    check =
      checkAnnotation typeEnv
  in
    case sortedDefs of
      Can.NoMain defs ->
        pure defs
          <* F.traverse_ check defs

      Can.YesMain before main after ->
        do  check main
              <* F.traverse_ check before
              <* F.traverse_ check after

            mainDef <- checkMain typeEnv main

            return (before ++ mainDef : after)



-- MISSING ANNOTATIONS


checkAnnotation :: TypeDict -> Can.Def -> Result ()
checkAnnotation typeEnv (Can.Def _ (A.A region pattern) _ maybeType) =
  case (pattern, maybeType) of
    (P.Var name, Nothing) ->
      let
        warning =
          Warning.MissingTypeAnnotation name (typeEnv ! name)
      in
        Result.warn region warning ()

    _ ->
      return ()



-- CHECK MAIN TYPE


checkMain :: TypeDict -> Can.Def -> Result Can.Def
checkMain typeEnv (Can.Def facts pattern@(A.A region _) body maybeType) =
  let
    mainType =
      typeEnv ! "main"

    makeError tipe maybeMsg =
      A.A region (Error.BadFlags tipe maybeMsg)

    getMainKind =
      case Type.deepDealias mainType of
        Type.App name [_]
          | name == vdomNode ->
              return Expr.VDom

        Type.App name [flags, _, _]
          | name == program && flags == never ->
              return Expr.NoFlags

          | name == program ->
              return (Expr.Flags flags)
                <* Effects.checkPortType makeError flags

        _ ->
          Result.throw region (Error.BadMain mainType)
  in
    do  kind <- getMainKind
        let newBody = A.A undefined (Expr.Program kind body)
        return (Can.Def facts pattern newBody maybeType)


program :: Type.Canonical
program =
  Type.Type (Var.inCore ["Platform"] "Program")


never :: Type.Canonical
never =
  Type.Type (Var.inCore ["Basics"] "Never")


vdomNode :: Type.Canonical
vdomNode =
  let
    vdom =
      ModuleName.Canonical Pkg.virtualDom ["VirtualDom"]
  in
    Type.Type (Var.fromModule vdom "Node")

