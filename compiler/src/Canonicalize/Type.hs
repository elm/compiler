{-# LANGUAGE Rank2Types #-}
module Canonicalize.Type
  ( canonicalize
  )
  where


import qualified Data.Dups as Dups
import qualified Data.List as List
import qualified Data.OneOrMore as OOM

import qualified Array as A

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Prim.Fields as F
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeIndex as T
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Canonicalize.Environment as Env
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as E



-- CANONICALIZE ANNOTATION


canonicalize :: Env.Env -> Src.Type -> (Can.Annotation -> a) -> (OOM.OneOrMore E.Problem -> a) -> a
canonicalize env srcType ok err =
  case canType env srcType of
    Result k ->
      k T.empty ok' err
  where
    ok' vars tipe =
      ok $ Can.Forall (T.toArray vars) tipe



-- CANONICALIZE TYPES


canType :: Env.Env -> Src.Type -> Result Can.Type
canType env =
    go
  where
    go (A.At region tipe) =
      A.At region <$>
      case tipe of
        Src.TVar x             -> canVar x
        Src.TType     r   n xs -> Env.withType     env r   n (canUnion env region r n xs) (canAlias env region r n xs) throw
        Src.TTypeQual r h n xs -> Env.withTypeQual env r h n (canUnion env region r n xs) (canAlias env region r n xs) throw
        Src.TLambda x y ys     -> Can.TLambda <$> go x <*> go y <*> traverse go ys
        Src.TUnit              -> return Can.TUnit
        Src.TTuple a b cs      -> goTuple region a b cs
        Src.TEmpty             -> return Can.TEmpty
        Src.TRecord f fs       -> Can.TRecord <$> canFields env f fs
        Src.TExtRec f fs e     -> Can.TExtRec <$> canFields env f fs <*> canExt e

    goTuple r a b cs =
      case cs of
        []  -> Can.TPair   <$> go a <*> go b
        [c] -> Can.TTriple <$> go a <*> go b <*> go c
        _   -> throw $ E.TupleLargerThanThree r



-- CANONICALIZE VAR


canVar :: T.Var -> Result Can.Type_
canVar var =
  Result $ \vars ok _ ->
    case T.register var vars of
      (i,vs) ->
        ok vs (Can.TVar i)



-- CANONICALIZE UNION


canUnion :: Env.Env -> A.Region -> A.Region -> T.Name -> [Src.Type] -> ModuleName.Canonical -> Env.UnionInfo -> Result Can.Type_
canUnion env region nameRegion name srcArgs home (Env.UnionInfo mask (Env.Arity expectedArity)) =
  do  args <- A.fromList <$> traverse (canType env) srcArgs
      let actualArity = A.size args
      if expectedArity == actualArity
        then return $ Can.TType nameRegion home name mask args
        else throw $ E.BadArity_Type region name expectedArity actualArity



-- CANONICALIZE ALIAS


canAlias :: Env.Env -> A.Region -> A.Region -> T.Name -> [Src.Type] -> ModuleName.Canonical -> Env.AliasInfo -> Result Can.Type_
canAlias env region nameRegion name srcArgs home (Env.AliasInfo mask (Env.Arity expectedArity) aliasedType) =
  do  args <- A.fromList <$> traverse (canType env) srcArgs
      let actualArity = A.size args
      if expectedArity == actualArity
        then return $ Can.TAlias nameRegion home name mask args aliasedType
        else throw $ E.BadArity_Type region name expectedArity actualArity



-- CANONICALIZE RECORD


type SrcField =
  (A.Located N.Name, Src.Type)


canFields :: Env.Env -> SrcField -> [SrcField] -> Result (F.Fields Can.Type)
canFields env f fs =
  case Dups.detect1 (\_ _ t -> t) E.DuplicateField $ List.foldl' add (one f) fs of
    Dups.Detector k ->
      k (F.fromOrdered (canType env)) throws
  where
    one   (A.At r n, t) = Dups.one1    n r (F.Ordered 0 t)
    add d (A.At r n, t) = Dups.insert1 n r (F.Ordered (fromIntegral (Dups.size1 d)) t) d


canExt :: A.Located T.Var -> Result (A.Located T.Index)
canExt (A.At r ext) =
  Result $ \vars ok _ ->
    case T.register ext vars of
      (i,vs) ->
        ok vs (A.At r i)



-- RESULT


newtype Result a =
  Result (forall r. Vars -> (Vars -> a -> r) -> (OOM.OneOrMore E.Problem -> r) -> r)


type Vars =
  T.Registry T.Var


throw :: E.Problem -> Result a
throw x =
  Result $ \_ _ err ->
    err (OOM.one x)


throws :: OOM.OneOrMore E.Problem -> Result a
throws xs =
  Result $ \_ _ err ->
    err xs


instance Functor Result where
  {-# INLINE fmap #-}
  fmap func (Result k) =
    Result $ \vars ok err ->
      let
        okF vs arg = ok vs (func arg)
      in
      k vars okF err


instance Applicative Result where
  {-# INLINE pure #-}
  pure a =
    Result $ \vars ok _ ->
      ok vars a

  {-# INLINE (<*>) #-}
  (<*>) (Result kF) (Result kA) =
    Result $ \vars ok err ->
      let
        okF vs func =
          let
            okA v arg = ok v (func arg)
          in
          kA vs okA err

        errF xs =
          kA vars (\_ _ -> err xs) (err . OOM.more xs)
      in
      kF vars okF errF


instance Monad Result where
  {-# INLINE (>>=) #-}
  (>>=) (Result kA) callback =
    Result $ \vars ok err ->
      let
        okA vs a =
          case callback a of
            Result kB -> kB vs ok err
      in
      kA vars okA err
