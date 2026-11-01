{-# LANGUAGE BangPatterns, Rank2Types #-}
module Canonicalize.Pattern
  ( Result(..)
  , canonicalize
  , canTypedArgs
  )
  where


import qualified Data.Dups as Dups
import qualified Data.List as List
import qualified Data.List.Utils as List
import qualified Data.NonEmptyMap as NEM
import qualified Data.OneOrMore as OOM

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as T
import qualified Canonicalize.Environment as Env
import qualified Data.Index as Index
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as E



-- RESULTS


newtype Result a =
  Result (forall r. Vars -> (Vars -> a -> r) -> (OOM.OneOrMore E.Problem -> r) -> r)


type Vars =
  Dups.Dict N.Name A.Region ()



-- CANONICALIZE


canonicalize :: Env.Env -> Src.Pattern -> Result Can.Pattern
canonicalize env (A.At region pattern) =
  A.At region <$>
  case pattern of
    Src.PAnything          -> pure Can.PAnything
    Src.PUnit              -> pure Can.PUnit
    Src.PChr c             -> pure (Can.PChr c)
    Src.PStr s             -> pure (Can.PStr s)
    Src.PInt i             -> pure (Can.PInt (fromIntegral i)) -- TODO make overflow an error
    Src.PVar n             -> canVar region n
    Src.PAlias p n         -> canAlias env p n
    Src.PTuple a b cs      -> canTuple env region a b cs
    Src.PRecord fs         -> canRecord fs
    Src.PList ps           -> Can.PList <$> traverse (canonicalize env) ps
    Src.PCons p ps         -> Can.PCons <$> canonicalize env p <*> canonicalize env ps
    Src.PCtor r n ps       -> Env.withCtor     env r   n (canVariant env region n ps) (badCtor r n) throw
    Src.PCtorQual r h n ps -> Env.withCtorQual env r h n (canVariant env region n ps) (badCtor r n) throw
  where
    badCtor r n _ _ =
      throw $ E.PatternHasRecordCtor r n



-- CANONICALIZE VAR


canVar :: A.Region -> N.Name -> Result Can.Pattern_
canVar r n =
  Result $ \vars ok _ ->
    ok (Dups.insert n r () vars) (Can.PVar n)



-- CANONICALIZE ALIAS


canAlias :: Env.Env -> Src.Pattern -> A.Located N.Name -> Result Can.Pattern_
canAlias env pattern (A.At r n) =
  Result $ \vars ok err ->
    case canonicalize env pattern of
      Result k ->
        let
          ok1 vs p = ok (Dups.insert n r () vs) (Can.PAlias p n)
        in
        k vars ok1 err



-- CANONICALIZE TUPLE


canTuple :: Env.Env -> A.Region -> Src.Pattern -> Src.Pattern -> [Src.Pattern] -> Result Can.Pattern_
canTuple env region a b cs =
  case cs of
    []  -> Can.PPair   <$> canonicalize env a <*> canonicalize env b
    [c] -> Can.PTriple <$> canonicalize env a <*> canonicalize env b <*> canonicalize env c
    _   -> throw $ E.TupleLargerThanThree region



-- CANONICALIZE RECORD


canRecord :: [A.Located N.Name] -> Result Can.Pattern_
canRecord fields =
  case fields of
    [] ->
      pure Can.PAnything

    f:fs ->
      Result $ \vars ok _ ->
        let
          !newVars = List.foldl' add (add vars f) fs
          !pattern = Can.PRecord $ NEM.fromList (go f) (List.map go fs)
        in
        ok newVars pattern
  where
    go       (A.At _ n) = (n, ())
    add dict (A.At r n) = Dups.insert n r () dict



-- CANONICALIZE VArIANT


canVariant :: Env.Env -> A.Region -> N.Name -> [Src.Pattern] -> ModuleName.Canonical -> Env.UnionCtor -> Result Can.Pattern_
canVariant env region name patterns _ ctor =
  let
    go index tipe pattern =
      Can.PArg index tipe <$> canonicalize env pattern
  in
  case ctor of
    Env.Variant _ ptype tag _ argTypes ->
      case Index.indexedZipWith go argTypes patterns of
        Index.LengthMatch results     -> Can.PVariant name ptype tag <$> sequenceA results
        Index.LengthMismatch want got -> throw $ E.BadArity_Pattern region name want got

    Env.Enum _ ptype tag ->
      case patterns of
        _:_ -> throw $ E.BadArity_Pattern region name 0 (List.length patterns)
        [] ->
          if isBool ptype
          then pure $ Can.PBool (name == N.true) name tag (Can._p_variants ptype)
          else pure $ Can.PEnum name ptype tag

    Env.Vector _ ptype _ x y zs ->
      case Index.indexedZipWith2 go x y zs patterns of
        Index.LengthMatch2 p q rs      -> Can.PVector name ptype <$> p <*> q <*> sequenceA rs
        Index.LengthMismatch2 want got -> throw $ E.BadArity_Pattern region name want got

    Env.Box _ ptype argType ->
      case patterns of
        [p] -> Can.PBox name ptype argType <$> canonicalize env p
        _   -> throw $ E.BadArity_Pattern region name 1 (List.length patterns)

    Env.Void _ ptype ->
      case patterns of
        []  -> pure $ Can.PVoid name ptype
        _:_ -> throw $ E.BadArity_Pattern region name 0 (List.length patterns)


isBool :: Can.PType -> Bool
isBool (Can.PType h n _ _ _) =
  n == T.bool && h == ModuleName.basics



-- CANONICALIZE TYPED ARGS


canTypedArgs :: Env.Env -> N.Name -> Src.Pattern -> [Src.Pattern] -> I.Type -> Result (Can.PArg, [Can.PArg], I.Type)
canTypedArgs env name arg args srcType =
    unroll arg args srcType Index.first []
  where
    unroll p ps tipe i revs =
      case tipe of
        I.TLambda t x xs               -> gather ps x xs (Index.next i) (canArg i t p) revs
        I.TAlias _ _ _ _ (I.Aliased t) -> unroll p ps t i revs
        I.TVar _                       -> tooShort i
        I.TType _ _ _ _                -> tooShort i
        I.TUnit                        -> tooShort i
        I.TPair _ _                    -> tooShort i
        I.TTriple _ _ _                -> tooShort i
        I.TEmpty                       -> tooShort i
        I.TRecord _                    -> tooShort i
        I.TExtRec _ _                  -> tooShort i

    gather patterns t ts i rev revs =
      case patterns of
        [] ->
          let (r,rs) = List.reverse1 rev revs in
          finish t ts <$> r <*> sequenceA rs

        p:ps ->
          case ts of
            []   -> unroll p ps t i (rev:revs)
            x:xs -> gather ps x xs (Index.next i) (canArg i t p) (rev:revs)

    finish t ts p ps =
      case ts of
        []   -> (p, ps, t)
        x:xs -> (p, ps, I.TLambda t x xs)

    canArg i t p =
      Can.PArg i t <$> canonicalize env p

    tooShort index =
      let
        (A.At start _) = arg
        (A.At end   _) = List.last1 arg args
      in
      throw $ E.AnnotationTooShort (A.mergeRegions start end) name index (1 + List.length args)



-- RESULT HELPERS


throw :: E.Problem -> Result a
throw x =
  Result $ \_ _ err ->
    err (OOM.one x)


instance Functor Result where
  {-# INLINE fmap #-}
  fmap func (Result k) =
    Result $ \vars ok err ->
      let
        ok' v arg = ok v (func arg)
      in
      k vars ok' err


instance Applicative Result where
  {-# INLINE pure #-}
  pure a =
    Result $ \vars ok _ ->
      ok vars a

  {-# INLINE (<*>) #-}
  (<*>) (Result kF) (Result kA) =
    Result $ \vars ok err ->
      let
        okF vs func = kA vs (\v arg -> ok v (func arg)) err
        errF xs     = kA Dups.none (\_ _ -> err xs) (err . OOM.more xs)
      in
      kF vars okF errF
