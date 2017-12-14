{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Nitpick.TopLevelTypes
  ( check
  )
  where


import Control.Monad (foldM)
import qualified Data.Map as Map
import Data.Map ((!))

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Type as Type
import qualified Canonicalize.Effects as Effects
import qualified Elm.Name as N
import qualified Optimize.Names as Names
import qualified Optimize.Port as Port
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Main as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result
import qualified Reporting.Warning as W



-- CHECK TOP-LEVEL TYPES


type Result i w a =
  Result.Result i w Error.Error a


type Annotations =
  Map.Map N.Name Can.Annotation


type Mains =
  Map.Map ModuleName.Canonical Opt.Main


check :: Annotations -> Can.Module -> Result i [W.Warning] Mains
check annotations (Can.Module home _ _ decls _ _ _ _) =
  checkDecls home annotations decls Map.empty


checkDecls :: ModuleName.Canonical -> Annotations -> Can.Decls -> Mains -> Result i [W.Warning] Mains
checkDecls home annotations decls mains =
  case decls of
    Can.Declare def subDecls ->
      checkDecls home annotations subDecls =<<
        checkDef home annotations mains def

    Can.DeclareRec defs subDecls ->
      checkDecls home annotations subDecls =<<
        foldM (checkDef home annotations) mains defs

    Can.SaveTheEnvironment ->
      Result.ok mains



-- MISSING ANNOTATIONS


checkDef :: ModuleName.Canonical -> Annotations -> Mains -> Can.Def -> Result i [W.Warning] Mains
checkDef home annotations mains def =
  case def of
    Can.Def (A.At region name) _ _ ->
      do  let (Can.Forall _ annotation) = annotations ! name
          Result.warn $ W.MissingTypeAnnotation region name annotation
          checkMain region annotations home name mains

    Can.TypedDef (A.At region name) _ _ _ _ ->
      checkMain region annotations home name mains



-- CHECK MAIN TYPE


checkMain :: R.Region -> Annotations -> ModuleName.Canonical -> N.Name -> Mains -> Result i w Mains
checkMain region annotations home name mains =
  if name == N.main then
    do  mainType <- checkMainType region (annotations ! name)
        Result.ok $ Map.insert home mainType mains
  else
    Result.ok mains


checkMainType :: R.Region -> Can.Annotation -> Result i w Opt.Main
checkMainType region (Can.Forall _ tipe) =
  case Type.deepDealias tipe of
    Can.TType home name [_]
      | home == ModuleName.virtualDom && name == N.node ->
          Result.ok Opt.Static

    Can.TType home name [flags, message, _]
      | home == ModuleName.platform && name == N.program ->
          case Effects.checkPayload flags of
            Left (subType, invalidPayload) ->
              Result.throw (Error.BadFlags region subType invalidPayload)

            Right () ->
              let (_, _, flagDecoder) = Names.run (Port.toFlagsDecoder flags) in
              Result.ok (Opt.Dynamic flagDecoder message)

    _ ->
      Result.throw (Error.BadType region tipe)
