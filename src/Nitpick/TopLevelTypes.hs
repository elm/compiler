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
import qualified Canonicalize.Result as Result
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R
import qualified Reporting.Warning as W



-- CHECK TOP-LEVEL TYPES


type Result i w a =
  Result.Result i w Error.Error a


type Types =
  Map.Map N.Name Can.Type


type Mains =
  Map.Map ModuleName.Canonical Opt.Main


check :: Types -> Can.Module -> Result i [W.Warning] Mains
check types (Can.Module home _ _ decls _ _ _ _) =
  checkDecls home types decls Map.empty


checkDecls :: ModuleName.Canonical -> Types -> Can.Decls -> Mains -> Result i [W.Warning] Mains
checkDecls home types decls mains =
  case decls of
    Can.Declare def subDecls ->
      checkDecls home types subDecls =<<
        checkDef home types mains def

    Can.DeclareRec defs subDecls ->
      checkDecls home types subDecls =<<
        foldM (checkDef home types) mains defs

    Can.SaveTheEnvironment ->
      Result.ok mains



-- MISSING ANNOTATIONS


checkDef :: ModuleName.Canonical -> Types -> Mains -> Can.Def -> Result i [W.Warning] Mains
checkDef home types mains def =
  case def of
    Can.Def (A.At region name) _ _ ->
      do  Result.warn $ W.MissingTypeAnnotation region name (types ! name)
          checkMain region types home name mains

    Can.TypedDef (A.At region name) _ _ _ _ ->
      checkMain region types home name mains



-- CHECK MAIN TYPE


checkMain :: R.Region -> Types -> ModuleName.Canonical -> N.Name -> Mains -> Result i w Mains
checkMain region types home name mains =
  if name == N.main then
    do  mainType <- checkMainType region (types ! name)
        Result.ok $ Map.insert home mainType mains
  else
    Result.ok mains


checkMainType :: R.Region -> Can.Type -> Result i w Opt.Main
checkMainType region tipe =
  case Type.deepDealias tipe of
    Can.TType home name [_]
      | home == ModuleName.virtualDom && name == N.node ->
          Result.ok Opt.Static

    Can.TType home name [flags, _, _]
      | home == ModuleName.platform && name == N.program ->
          case Effects.checkPayload flags of
            Left (subType, invalidPayload) ->
              Result.throw (Error.MainFlags region subType invalidPayload)

            Right () ->
              Result.ok (Opt.Dynamic flags)

    _ ->
      Result.throw (Error.MainType region tipe)
