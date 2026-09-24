{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, Rank2Types, UnboxedTuples #-}
module Optimize.Names
  ( Tracker
  , run
  , generate
  , registerKernel
  , registerGlobal
  , registerDebug
  , registerCtor
  , registerField
  , registerFieldDict
  , registerFieldList
  --
  , fromVarIndex
  )
  where


import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Int (Int(..))
import GHC.Prim
import GHC.ST (ST(ST), runST)
import GHC.Word (Word8(..))

import qualified String as S

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified Data.Index as Index
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A



-- GENERATOR


newtype Tracker a =
  Tracker (
    forall r.
      Int
      -> Set.Set Opt.Global
      -> Map.Map N.Name Int
      -> (Int -> Set.Set Opt.Global -> Map.Map N.Name Int -> a -> r)
      -> r
  )


run :: Tracker a -> (Set.Set Opt.Global, Map.Map N.Name Int, a)
run (Tracker k) =
  k 0 Set.empty Map.empty
    (\_uid deps fields value -> (deps, fields, value))


generate :: Tracker N.Name
generate =
  Tracker $ \uid deps fields ok ->
    ok (uid + 1) deps fields (fromVarIndex uid)


registerKernel :: Module.Kernel -> a -> Tracker a
registerKernel home value =
  Tracker $ \uid deps fields ok ->
    ok uid (Set.insert (Opt.toKernelGlobal home) deps) fields value


registerGlobal :: ModuleName.Canonical -> N.Name -> Tracker Opt.Expr
registerGlobal home name =
  Tracker $ \uid deps fields ok ->
    let global = Opt.Global home name in
    ok uid (Set.insert global deps) fields (Opt.VarGlobal global)


registerDebug :: N.Name -> ModuleName.Canonical -> A.Region -> Tracker Opt.Expr
registerDebug name home region =
  Tracker $ \uid deps fields ok ->
    let global = Opt.Global ModuleName.debug name in
    ok uid (Set.insert global deps) fields (Opt.VarDebug name home region Nothing)


registerCtor :: ModuleName.Canonical -> N.Name -> Index.ZeroBased -> Can.CtorOpts -> Tracker Opt.Expr
registerCtor home name index opts =
  Tracker $ \uid deps fields ok ->
    let
      global = Opt.Global home name
      newDeps = Set.insert global deps
    in
    case opts of
      Can.Normal ->
        ok uid newDeps fields (Opt.VarGlobal global)

      Can.Enum ->
        ok uid newDeps fields $
          case () of
            _ | name == N.true  && home == ModuleName.basics -> Opt.Bool True
              | name == N.false && home == ModuleName.basics -> Opt.Bool False
              | otherwise                                    -> Opt.VarEnum global index

      Can.Unbox ->
        ok uid (Set.insert identity newDeps) fields (Opt.VarBox global)


identity :: Opt.Global
identity =
  Opt.Global ModuleName.basics N.identity


registerField :: N.Name -> a -> Tracker a
registerField name value =
  Tracker $ \uid d fields ok ->
    ok uid d (Map.insertWith (+) name 1 fields) value


registerFieldDict :: Map.Map N.Name v -> a -> Tracker a
registerFieldDict newFields value =
  Tracker $ \uid d fields ok ->
    ok uid d (Map.unionWith (+) fields (Map.map toOne newFields)) value


toOne :: a -> Int
toOne _ = 1


registerFieldList :: [N.Name] -> a -> Tracker a
registerFieldList names value =
  Tracker $ \uid deps fields ok ->
    ok uid deps (foldr addOne fields names) value


addOne :: N.Name -> Map.Map N.Name Int -> Map.Map N.Name Int
addOne name fields =
  Map.insertWith (+) name 1 fields



-- FROM VAR INDEX


fromVarIndex :: Int -> N.Name
fromVarIndex index =
  runST $ ST $ \s0 ->
    case newByteArray# len                        s0 of { (# s1, mba #) ->
    case writeWord8Array# mba 0# 0x5F#Word8 {-_-} s1 of {    s2         ->
    case writeWord8Array# mba 1# 0x76#Word8 {-v-} s2 of {    s3         ->
    case loop mba len index                       s3 of {    s4         ->
    case unsafeFreezeByteArray# mba               s4 of { (# s5, ba #) ->
      (# s5, N.fromString (S.String ba) #)
    }}}}}
  where
    !(I# len) = 2 + getIndexSize index

    loop mba oldOffset n s0 =
      case writeWord8Array# mba newOffset w s0 of
        s1 ->
          if q <= 0
          then s1
          else loop mba newOffset q s1
      where
        (q,r) = quotRem n 10
        !newOffset = oldOffset -# 1#
        !(W8# w) = 0x30 + fromIntegral r


getIndexSize :: Int -> Int
getIndexSize n
  | n < 10  = 1
  | n < 100 = 2
  | True    = ceiling (logBase 10 (fromIntegral n + 1) :: Float)



-- INSTANCES


instance Functor Tracker where
  fmap func (Tracker kv) =
    Tracker $ \n d f ok ->
      let
        ok1 n1 d1 f1 value =
          ok n1 d1 f1 (func value)
      in
      kv n d f ok1


instance Applicative Tracker where
  {-# INLINE pure #-}
  pure value =
    Tracker $ \n d f ok -> ok n d f value

  (<*>) (Tracker kf) (Tracker kv) =
    Tracker $ \n d f ok ->
      let
        ok1 n1 d1 f1 func =
          let
            ok2 n2 d2 f2 value =
              ok n2 d2 f2 (func value)
          in
          kv n1 d1 f1 ok2
      in
      kf n d f ok1


instance Monad Tracker where
  return = pure

  (>>=) (Tracker k) callback =
    Tracker $ \n d f ok ->
      let
        ok1 n1 d1 f1 a =
          case callback a of
            Tracker kb -> kb n1 d1 f1 ok
      in
      k n d f ok1
