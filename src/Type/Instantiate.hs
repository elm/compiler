{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Instantiate
  ( flexible
  , rigid
  , fromSrcType
  )
  where


import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))

import qualified AST.Canonical as Can
import qualified Elm.Name as N
import Type.Type



-- FROM SOURCE TYPES


-- TODO should the freeFlexVars be ranked a certain way?
-- How does this interact with `TS.flatten` exactly?
flexible :: Can.Type -> IO ( Type, Map.Map N.Name Variable )
flexible srcType =
  do  let freeVars = gatherFreeVars srcType Map.empty
      freeFlexVars <- Map.traverseWithKey (\name () -> nameToFlex name) freeVars
      tipe <- fromSrcType (Map.map VarN freeFlexVars) srcType
      return ( tipe, freeFlexVars )


rigid :: Can.Type -> IO ( Type, Map.Map N.Name Variable )
rigid srcType =
  do  let freeVars = gatherFreeVars srcType Map.empty
      freeRigidVars <- Map.traverseWithKey (\name () -> nameToRigid name) freeVars
      tipe <- fromSrcType (Map.map VarN freeRigidVars) srcType
      return ( tipe, freeRigidVars )



-- GATHER FREE VARS


gatherFreeVars :: Can.Type -> Map.Map N.Name () -> Map.Map N.Name ()
gatherFreeVars tipe dict =
  case tipe of
    Can.TLambda arg result ->
      gatherFreeVars result (gatherFreeVars arg dict)

    Can.TVar name ->
      Map.insert name () dict

    Can.TType _ _ args ->
      foldr gatherFreeVars dict args

    Can.TAlias _ _ args _ ->
      foldr gatherFreeVars dict (map snd args)

    Can.TTuple a b maybeC ->
      gatherFreeVars a $ gatherFreeVars b $
        case maybeC of
          Nothing ->
             dict

          Just c ->
            gatherFreeVars c dict

    Can.TUnit ->
      dict

    Can.TRecord fields maybeExt ->
      case maybeExt of
        Nothing ->
          Map.foldr gatherFreeVars dict fields

        Just ext ->
          Map.foldr gatherFreeVars (gatherFreeVars ext dict) fields



-- FROM SOURCE TYPE


fromSrcType :: Map.Map N.Name Type -> Can.Type -> IO Type
fromSrcType freeVars sourceType =
  case sourceType of
    Can.TLambda arg result ->
      FunN
        <$> fromSrcType freeVars arg
        <*> fromSrcType freeVars result

    Can.TVar name ->
      return (freeVars ! name)

    Can.TType home name args ->
      AppN home name <$> traverse (fromSrcType freeVars) args

    Can.TAlias home name args aliasedType ->
      do  targs <- traverse (traverse (fromSrcType freeVars)) args
          AliasN home name targs <$>
            case aliasedType of
              Can.Filled realType ->
                fromSrcType freeVars realType

              Can.Holey realType ->
                fromSrcType (Map.fromList targs) realType

    Can.TTuple a b maybeC ->
      TupleN
        <$> fromSrcType freeVars a
        <*> fromSrcType freeVars b
        <*> traverse (fromSrcType freeVars) maybeC

    Can.TUnit ->
      return UnitN

    Can.TRecord fields maybeExt ->
      RecordN
        <$> traverse (fromSrcType freeVars) fields
        <*>
          case maybeExt of
            Nothing ->
              return EmptyRecordN

            Just ext ->
              fromSrcType freeVars ext
