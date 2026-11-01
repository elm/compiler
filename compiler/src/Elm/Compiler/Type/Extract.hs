{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, Rank2Types, TemplateHaskell #-}
module Elm.Compiler.Type.Extract
  ( fromAnnotation
  , fromType
  , Types(..)
  , mergeMany
  , merge
  , fromInterface
  , fromDependencyInterface
  , fromMsg
  )
  where


import qualified Data.Map as Map
import qualified Data.Map.Utils as Map
import qualified Data.Set as Set

import qualified Array as A
import qualified String as S

import qualified AST.Prim.Fields as F
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Tag as Tag
import qualified AST.Prim.TypeIndex as T
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Elm.Compiler.Type as CT
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName



-- EXTRACTION


fromAnnotation :: I.Annotation -> CT.Type
fromAnnotation (I.Forall args tipe) =
  fromType args tipe


fromType :: A.Array T.Var -> I.Type -> CT.Type
fromType args tipe =
  snd (run (extract args tipe))


extract :: A.Array T.Var -> I.Type -> Extractor CT.Type
extract args =
    go
  where
    go tipe =
      case tipe of
        I.TLambda x y zs    -> CT.Lambda <$> go x <*> go y <*> traverse go zs
        I.TVar x            -> pure (CT.Var (T.get x args))
        I.TType h n _ xs    -> addUnion (Seen h n) (CT.Type (toPublicName h n)) <*> traverse go (A.toList xs)
        I.TUnit             -> pure CT.Unit
        I.TPair   a b       -> CT.Tuple <$> go a <*> go b <*> pure []
        I.TTriple a b c     -> CT.Tuple <$> go a <*> go b <*> fmap (:[]) (go c)
        I.TEmpty            -> pure CT.Empty
        I.TRecord flds      -> let (f,fs) = F.flatten flds in CT.Record <$> traverse go f <*> traverse (traverse go) fs
        I.TExtRec flds e    -> let (f,fs) = F.flatten flds in CT.ExtRec <$> traverse go f <*> traverse (traverse go) fs <*> pure (T.get e args)
        I.TAlias h n _ xs a ->
          do  addAlias (Seen h n) ()
              _ <- go (I.dealias xs a)
              CT.Type (toPublicName h n) <$> traverse go (A.toList xs)


toPublicName :: ModuleName.Canonical -> T.Name -> T.Name
toPublicName (ModuleName.Canonical _ home) name =
  T.nameFromString $
    S.join (Module.toString home) 0x2E#Word8 {-.-} (T.nameToString name)



-- TRANSITIVELY AVAILABLE TYPES


newtype Types =
  Types (Map.Map ModuleName.Canonical Types_)
  -- PERF profile Seen representation
  -- current representation needs less allocation
  -- but maybe the lookup is much worse


data Types_ =
  Types_
    { _union_info :: Map.Map T.Name I.Union
    , _alias_info :: Map.Map T.Name I.Alias
    }


mergeMany :: [Types] -> Types
mergeMany listOfTypes =
  case listOfTypes of
    []   -> Types Map.empty
    t:ts -> foldr merge t ts


merge :: Types -> Types -> Types
merge (Types types1) (Types types2) =
  Types (Map.union types1 types2)


fromInterface :: Module.Name -> I.Interface -> Types
fromInterface name (I.Interface pkg _ types _) =
    Types $ Map.singleton (ModuleName.Canonical pkg name) $
      uncurry Types_ $ Map.mapEither (I.extract Left Right) types


fromDependencyInterface :: ModuleName.Canonical -> I.DependencyInterface -> Types
fromDependencyInterface home di =
  Types $ Map.singleton home $
    case di of
      I.Public (I.Interface _ _ types _) ->
        uncurry Types_ $ Map.mapEither (I.extract Left Right) types

      I.Private _ types ->
        uncurry Types_ $ Map.mapEither go types
  where
    go t =
      case t of
        I.PrivateUnion u -> Left u
        I.PrivateAlias a -> Right a



-- EXTRACT MODEL, MSG, AND ANY TRANSITIVE DEPENDENCIES


fromMsg :: Types -> A.Array T.Var -> I.Type -> CT.DebugMetadata
fromMsg types args message =
  let
    (msgDeps, msgType) = run (extract args message)
    (aliases, unions ) = extractTransitive types noDeps msgDeps
  in
  CT.DebugMetadata msgType aliases unions


extractTransitive :: Types -> Deps -> Deps -> ( [CT.Alias], [CT.Union] )
extractTransitive types (Deps seenAliases seenUnions) (Deps nextAliases nextUnions) =
  let
    aliases = Set.difference nextAliases seenAliases
    unions  = Set.difference nextUnions seenUnions
  in
    if Set.null aliases && Set.null unions then
      ( [], [] )

    else
      let
        (newDeps, result) =
          run $
            (,)
              <$> traverse (extractAlias types) (Set.toList aliases)
              <*> traverse (extractUnion types) (Set.toList unions)

        oldDeps =
          Deps (Set.union seenAliases nextAliases) (Set.union seenUnions nextUnions)

        remainingResult =
          extractTransitive types oldDeps newDeps
      in
        mappend result remainingResult


extractAlias :: Types -> Seen -> Extractor CT.Alias
extractAlias (Types dict) (Seen h n) =
  let
    types                        = $(Map.require 'extractAlias) h dict (Module.toChars . ModuleName._module)
    (I.Alias _ xs (I.Aliased t)) = $(Map.require 'extractAlias) n (_alias_info types) T.nameToChars
  in
  CT.Alias (toPublicName h n) (A.toList xs) <$> extract xs t


extractUnion :: Types -> Seen -> Extractor CT.Union
extractUnion (Types dict) (Seen home name) =
  if name == T.list && home == ModuleName.list
  then return $ CT.Union (toPublicName home name) [T.a] []
  else
    let
      types                   = $(Map.require 'extractUnion) home dict (Module.toChars . ModuleName._module)
      (I.Union _ xs variants) = $(Map.require 'extractUnion) name (_union_info types) T.nameToChars
    in
    CT.Union (toPublicName home name) (A.toList xs) <$>
      case variants of
        I.Variants     vs -> sequence $ Tag.unpack vs (\_ bs -> bs) (\_ (I.Variant n ts) -> go xs n ts)
        I.Enum         es -> pure     $ Tag.unpack es (\_ bs -> bs) (\_            n     -> (n,[]) )
        I.Vector n a b cs -> (:[]) <$> go xs n (a:b:cs)
        I.Box    n t      -> (:[]) <$> go xs n [t]
        I.Void   n        -> pure [(n,[])]
  where
    go xs n args =
      (,) n <$> traverse (extract xs) args



-- DEPS


data Deps =
  Deps
    { _aliases :: Set.Set Seen
    , _unions :: Set.Set Seen
    }


data Seen = Seen ModuleName.Canonical T.Name
  deriving (Eq, Ord)


{-# NOINLINE noDeps #-}
noDeps :: Deps
noDeps =
  Deps Set.empty Set.empty



-- EXTRACTOR


newtype Extractor a =
  Extractor (
    forall result.
      Set.Set Seen
      -> Set.Set Seen
      -> (Set.Set Seen -> Set.Set Seen -> a -> result)
      -> result
  )


run :: Extractor a -> (Deps, a)
run (Extractor k) =
  k Set.empty Set.empty $ \aliases unions value ->
    ( Deps aliases unions, value )


addAlias :: Seen -> a -> Extractor a
addAlias alias value =
  Extractor $ \aliases unions ok ->
    ok (Set.insert alias aliases) unions value


addUnion :: Seen -> a -> Extractor a
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
