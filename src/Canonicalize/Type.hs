{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Type
  ( canonicalize
  )
  where


import qualified Data.Map as Map

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified Canonicalize.Environment.Dups as Dups
import qualified Canonicalize.Environment.Internals as Env
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



-- RESULT


type Result i a =
  Result.Result i Warning.Warning Error.Error a



-- CANONICALIZE TYPES


canonicalize :: Env.Env -> Src.Type -> Result () Can.Type
canonicalize env (A.A typeRegion tipe) =
  case tipe of
    Src.TVar x ->
        Result.ok (Can.TVar x)

    Src.TType region maybePrefix name args ->
        do  info <- Env.findType region env maybePrefix name
            cargs <- traverse (canonicalize env) args
            case info of
              Env.Alias arity home argNames aliasedType ->
                checkArgs Error.AliasArgs arity typeRegion name args $
                  Can.TAlias home name (zip argNames cargs) (Can.Holey aliasedType)

              Env.Union arity home ->
                checkArgs Error.UnionArgs arity typeRegion name args $
                  Can.TType home name cargs

    Src.TLambda a b ->
        Can.TLambda
          <$> canonicalize env a
          <*> canonicalize env b

    Src.TRecord fields ext ->
        do  fieldDict <- Dups.checkFields fields
            Can.TRecord
              <$> Map.traverseWithKey (\_ t -> canonicalize env t) fieldDict
              <*> traverse (canonicalize env) ext

    Src.TUnit ->
        Result.ok Can.TUnit

    Src.TTuple a b cs ->
        Can.TTuple
          <$> canonicalize env a
          <*> canonicalize env b
          <*>
            case cs of
              [] ->
                return Nothing

              [c] ->
                Just <$> canonicalize env c

              _ ->
                let (A.A start _, A.A end _) = (head cs, last cs) in
                Result.throw typeRegion $
                  Error.TupleLargerThanThree (R.merge start end)



-- CHECK ARGS


checkArgs :: Error.Args -> Int -> R.Region -> N.Name -> [A.Located arg] -> answer -> Result () answer
checkArgs argsType expected region name args answer =
  let actual = length args in
  if expected == actual then
    Result.ok answer
  else
    Result.throw region $
      if actual < expected then
        Error.TooFew argsType name expected actual
      else
        let
          extras = drop expected args
          (A.A start _, A.A end _) = (head extras, last extras)
        in
        Error.TooMany argsType name expected actual (R.merge start end)
