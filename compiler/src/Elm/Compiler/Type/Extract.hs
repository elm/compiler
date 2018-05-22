{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
module Elm.Compiler.Type.Extract
  ( fromAnnotation
  , fromType
  , fromMsg
  )
  where


import Data.Map ((!))
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Type as Type
import qualified Elm.Compiler.Type as T
import qualified Elm.Interface as I
import qualified Elm.Name as N



-- EXTRACTION


fromAnnotation :: Can.Annotation -> T.Type
fromAnnotation (Can.Forall _ astType) =
  fromType astType


fromType :: Can.Type -> T.Type
fromType astType =
  snd (run (extract astType))


extract :: Can.Type -> Extractor T.Type
extract astType =
  case astType of
    Can.TLambda arg result ->
      T.Lambda
        <$> extract arg
        <*> extract result

    Can.TVar x ->
      pure (T.Var x)

    Can.TType home name args ->
      addUnion (Opt.Global home name) (T.Type (toPublicName home name))
        <*> traverse extract args

    Can.TRecord fields ext ->
      do  efields <- traverse (traverse extract) (Can.fieldsToList fields)
          pure (T.Record efields ext)

    Can.TUnit ->
      pure T.Unit

    Can.TTuple a b maybeC ->
      T.Tuple
        <$> extract a
        <*> extract b
        <*> traverse extract (Maybe.maybeToList maybeC)

    Can.TAlias home name args aliasType ->
      do  addAlias (Opt.Global home name) ()
          _ <- extract (Type.dealias args aliasType)
          T.Type (toPublicName home name)
            <$> traverse (extract . snd) args


toPublicName :: ModuleName.Canonical -> N.Name -> N.Name
toPublicName (ModuleName.Canonical _ home) name =
  N.sepBy 0x2E {- . -} home name



-- EXTRACT MODEL, MSG, AND ANY TRANSITIVE DEPENDENCIES


fromMsg :: I.Interfaces -> Can.Type -> T.DebugMetadata
fromMsg interfaces message =
  let
    (msgDeps, msgType) =
      run (extract message)

    (aliases, unions) =
      extractTransitive interfaces noDeps msgDeps
  in
  T.DebugMetadata msgType aliases unions


extractTransitive :: I.Interfaces -> Deps -> Deps -> ( [T.Alias], [T.Union] )
extractTransitive interfaces (Deps seenAliases seenUnions) (Deps nextAliases nextUnions) =
  let
    aliases = Set.difference nextAliases seenAliases
    unions = Set.difference nextUnions seenUnions
  in
    if Set.null aliases && Set.null unions then
      ( [], [] )

    else
      let
        (newDeps, result) =
          run $
            (,)
              <$> traverse (extractAlias interfaces) (Set.toList aliases)
              <*> traverse (extractUnion interfaces) (Set.toList unions)

        oldDeps =
          Deps (Set.union seenAliases nextAliases) (Set.union seenUnions nextUnions)

        remainingResult =
          extractTransitive interfaces oldDeps newDeps
      in
        mappend result remainingResult


extractAlias :: I.Interfaces -> Opt.Global -> Extractor T.Alias
extractAlias interfaces (Opt.Global home name) =
  let
    (I.Interface _ _ aliases _) = interfaces ! home
    (Can.Alias args aliasType) = I.toAliasInternals (aliases ! name)
  in
  T.Alias (toPublicName home name) args <$> extract aliasType


extractUnion :: I.Interfaces -> Opt.Global -> Extractor T.Union
extractUnion interfaces (Opt.Global home name) =
  if name == N.list && home == ModuleName.list
    then return $ T.Union (toPublicName home name) ["a"] []
    else
      let
        pname = toPublicName home name
        unions = I._unions (interfaces ! home)
      in
      case I.toUnionInternals (unions ! name) of
        Can.Union vars ctors _ _ ->
          T.Union pname vars <$> traverse extractCtor ctors


extractCtor :: Can.Ctor -> Extractor (N.Name, [T.Type])
extractCtor (Can.Ctor ctor _ _ args) =
  (,) ctor <$> traverse extract args



-- DEPS


data Deps =
  Deps
    { _aliases :: Set.Set Opt.Global
    , _unions :: Set.Set Opt.Global
    }


{-# NOINLINE noDeps #-}
noDeps :: Deps
noDeps =
  Deps Set.empty Set.empty



-- EXTRACTOR


newtype Extractor a =
  Extractor (
    forall result.
      Set.Set Opt.Global
      -> Set.Set Opt.Global
      -> (Set.Set Opt.Global -> Set.Set Opt.Global -> a -> result)
      -> result
  )


run :: Extractor a -> (Deps, a)
run (Extractor k) =
  k Set.empty Set.empty $ \aliases unions value ->
    ( Deps aliases unions, value )


addAlias :: Opt.Global -> a -> Extractor a
addAlias alias value =
  Extractor $ \aliases unions ok ->
    ok (Set.insert alias aliases) unions value


addUnion :: Opt.Global -> a -> Extractor a
addUnion union value =
  Extractor $ \aliases unions ok ->
    ok aliases (Set.insert union unions) value


instance Functor Extractor where
  fmap func (Extractor k) =
    Extractor $ \aliases unions ok ->
      let
        ok1 a1 u1 value =
          ok a1 u1 (func value)
      in
      k aliases unions ok1


instance Applicative Extractor where
  pure value =
    Extractor $ \aliases unions ok ->
      ok aliases unions value

  (<*>) (Extractor kf) (Extractor kv) =
    Extractor $ \aliases unions ok ->
      let
        ok1 a1 u1 func =
          let
            ok2 a2 u2 value =
              ok a2 u2 (func value)
          in
          kv a1 u1 ok2
      in
      kf aliases unions ok1


instance Monad Extractor where
  return = pure

  (>>=) (Extractor ka) callback =
    Extractor $ \aliases unions ok ->
      let
        ok1 a1 u1 value =
          case callback value of
            Extractor kb -> kb a1 u1 ok
      in
      ka aliases unions ok1
