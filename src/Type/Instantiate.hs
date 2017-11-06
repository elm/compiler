{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Instantiate
  ( Ctor(..)
  , pattern
  , flexible
  , rigid
  )
  where


import Control.Arrow (second)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))

import qualified AST.Expression.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as T
import qualified Elm.Name as N
import Type.Type



-- FROM PATTERN


data Ctor =
  Ctor
    { _vars :: [Variable]
    , _args :: [(Int, Type, Can.Pattern)]
    , _type :: Type
    }


pattern :: ModuleName.Canonical -> N.Name -> [N.Name] -> [(Int, T.Canonical, Can.Pattern)] -> IO Ctor
pattern home tipe tvars args =
  do  varPairs <- traverse (\var -> (,) var <$> nameToFlex var) tvars
      let typePairs = map (second VarN) varPairs

      targs <- traverse (patternHelp (Map.fromList typePairs)) args

      return $
        Ctor
          { _vars = map snd varPairs
          , _args = targs
          , _type = AppN home tipe (map snd typePairs)
          }


patternHelp :: Map.Map N.Name Type -> (i, T.Canonical, p) -> IO (i, Type, p)
patternHelp freeVarDict (index, srcType, arg) =
  do  tipe <- fromSrcType freeVarDict srcType
      return (index, tipe, arg)



-- FROM SOURCE TYPES


-- TODO should the freeFlexVars be ranked a certain way?
-- How does this interact with `TS.flatten` exactly?
flexible :: T.Canonical -> IO ( Type, Map.Map N.Name Variable )
flexible srcType =
  do  let freeVars = gatherFreeVars srcType Map.empty
      freeFlexVars <- Map.traverseWithKey (\name () -> nameToFlex name) freeVars
      tipe <- fromSrcType (Map.map VarN freeFlexVars) srcType
      return ( tipe, freeFlexVars )


rigid :: T.Canonical -> IO ( Type, Map.Map N.Name Variable )
rigid srcType =
  do  let freeVars = gatherFreeVars srcType Map.empty
      freeRigidVars <- Map.traverseWithKey (\name () -> nameToRigid name) freeVars
      tipe <- fromSrcType (Map.map VarN freeRigidVars) srcType
      return ( tipe, freeRigidVars )



-- GATHER FREE VARS


gatherFreeVars :: T.Canonical -> Map.Map N.Name () -> Map.Map N.Name ()
gatherFreeVars tipe dict =
  case tipe of
    T.Lambda arg result ->
      gatherFreeVars result (gatherFreeVars arg dict)

    T.Var name ->
      Map.insert name () dict

    T.Type _ _ args ->
      foldr gatherFreeVars dict args

    T.Aliased _ _ args _ ->
      foldr gatherFreeVars dict (map snd args)

    T.Tuple a b maybeC ->
      gatherFreeVars a $ gatherFreeVars b $
        case maybeC of
          Nothing ->
             dict

          Just c ->
            gatherFreeVars c dict

    T.Unit ->
      dict

    T.Record fields maybeExt ->
      case maybeExt of
        Nothing ->
          Map.foldr gatherFreeVars dict fields

        Just ext ->
          Map.foldr gatherFreeVars (gatherFreeVars ext dict) fields



-- FROM SOURCE TYPE


fromSrcType :: Map.Map N.Name Type -> T.Canonical -> IO Type
fromSrcType freeVars sourceType =
  case sourceType of
    T.Lambda arg result ->
      FunN
        <$> fromSrcType freeVars arg
        <*> fromSrcType freeVars result

    T.Var name ->
      return (freeVars ! name)

    T.Type home name args ->
      AppN home name <$> traverse (fromSrcType freeVars) args

    T.Aliased home name args aliasedType ->
      do  targs <- traverse (traverse (fromSrcType freeVars)) args
          AliasN home name targs <$>
            case aliasedType of
              T.Filled realType ->
                fromSrcType freeVars realType

              T.Holey realType ->
                fromSrcType (Map.fromList targs) realType

    T.Tuple a b maybeC ->
      TupleN
        <$> fromSrcType freeVars a
        <*> fromSrcType freeVars b
        <*> traverse (fromSrcType freeVars) maybeC

    T.Unit ->
      return UnitN

    T.Record fields maybeExt ->
      RecordN
        <$> traverse (fromSrcType freeVars) fields
        <*>
          case maybeExt of
            Nothing ->
              return EmptyRecordN

            Just ext ->
              fromSrcType freeVars ext
