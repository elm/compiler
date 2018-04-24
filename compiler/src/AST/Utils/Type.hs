{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Utils.Type
  ( delambda
  , dealias
  , deepDealias
  , iteratedDealias
  )
  where


import qualified Data.Map as Map

import AST.Canonical (Type(..), AliasType(..), FieldType(..))
import qualified Elm.Name as N



-- DELAMBDA


delambda :: Type -> [Type]
delambda tipe =
  case tipe of
    TLambda arg result ->
      arg : delambda result

    _ ->
      [tipe]



-- DEALIAS


dealias :: [(N.Name, Type)] -> AliasType -> Type
dealias args aliasType =
  case aliasType of
    Holey tipe ->
      dealiasHelp (Map.fromList args) tipe

    Filled tipe ->
      tipe


dealiasHelp :: Map.Map N.Name Type -> Type -> Type
dealiasHelp typeTable tipe =
  case tipe of
    TLambda a b ->
      TLambda
        (dealiasHelp typeTable a)
        (dealiasHelp typeTable b)

    TVar x ->
      Map.findWithDefault tipe x typeTable

    TRecord fields ext ->
      TRecord (Map.map (dealiasField typeTable) fields) ext

    TAlias home name args t' ->
      TAlias home name (map (fmap (dealiasHelp typeTable)) args) t'

    TType home name args ->
      TType home name (map (dealiasHelp typeTable) args)

    TUnit ->
      TUnit

    TTuple a b maybeC ->
      TTuple
        (dealiasHelp typeTable a)
        (dealiasHelp typeTable b)
        (fmap (dealiasHelp typeTable) maybeC)


dealiasField :: Map.Map N.Name Type -> FieldType -> FieldType
dealiasField typeTable (FieldType index tipe) =
  FieldType index (dealiasHelp typeTable tipe)



-- DEEP DEALIAS


deepDealias :: Type -> Type
deepDealias tipe =
  case tipe of
    TLambda a b ->
      TLambda (deepDealias a) (deepDealias b)

    TVar _ ->
      tipe

    TRecord fields ext ->
      TRecord (Map.map deepDealiasField fields) ext

    TAlias _ _ args tipe' ->
      deepDealias (dealias args tipe')

    TType home name args ->
      TType home name (map deepDealias args)

    TUnit ->
      TUnit

    TTuple a b c ->
      TTuple (deepDealias a) (deepDealias b) (fmap deepDealias c)


deepDealiasField :: FieldType -> FieldType
deepDealiasField (FieldType index tipe) =
  FieldType index (deepDealias tipe)



-- ITERATED DEALIAS


iteratedDealias :: Type -> Type
iteratedDealias tipe =
  case tipe of
    TAlias _ _ args realType ->
      iteratedDealias (dealias args realType)

    _ ->
      tipe
