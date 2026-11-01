{-# LANGUAGE BangPatterns, Rank2Types, TemplateHaskell #-}
module Canonicalize.Environment.Foreign
  ( Result(..)
  , initialize
  )
  where


import Prelude hiding (init)
import qualified Data.List as List
import qualified Data.List.Utils as List
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Utils as Map
import qualified Data.OneOrMore as OOM

import qualified Array as A
import qualified Crash

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Prim.Fields as F
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.Tag as Tag
import qualified AST.Prim.TypeIndex as T
import qualified AST.Prim.TypeMask as M
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Canonicalize.Environment as Env
import Canonicalize.Environment (Type(..), UnionInfo(..), AliasInfo(..), UnionCtor(..), AliasCtor(..))
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Nitpick.Utils as Nit
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as E



-- RESULT


newtype Result a =
  Result (forall r. (a -> r) -> (OOM.OneOrMore E.Error -> r) -> r)


initialize :: ModuleName.Canonical -> Map.Map Module.Name I.Interface -> [Src.Import] -> Result Env.Env
initialize home ifaces imports =
  toEnv home . List.foldr merge empty <$> traverse (init ifaces) (toSafeImports home imports)



-- TO ENV


toEnv :: ModuleName.Canonical -> State -> Env.Env
toEnv home (State vs ts cs bs qvs qts qcs) =
  Env.Env home (Map.map toValue vs) ts cs bs qvs qts qcs


toValue :: Env.ForeignValue -> Env.Value
toValue info =
  case info of
    Env.ForeignValue  h t -> Env.Foreign h t
    Env.ForeignValues amb -> Env.Foreigns amb



-- STATE


data State =
  State
    { _vals    :: Map.Map N.Name Env.ForeignValue
    , _types   :: Map.Map T.Name (Type UnionInfo AliasInfo)
    , _ctors   :: Map.Map N.Name (Type UnionCtor AliasCtor)
    , _binops  :: Map.Map Op.Name Env.Binop
    , _q_vals  :: Map.Map Module.Prefix (Map.Map N.Name Env.ForeignValue)
    , _q_types :: Map.Map Module.Prefix (Map.Map T.Name (Type UnionInfo AliasInfo))
    , _q_ctors :: Map.Map Module.Prefix (Map.Map N.Name (Type UnionCtor AliasCtor))
    }


empty :: State
empty =
    State Map.empty emptyTypes Map.empty Map.empty Map.empty Map.empty Map.empty
  where
    emptyTypes =
      Map.singleton T.list $
        Env.Union ModuleName.list (Env.UnionInfo M.list (Env.Arity 1))


merge :: State -> State -> State
merge (State vs1 ts1 cs1 bs1 qvs1 qts1 qcs1) (State vs2 ts2 cs2 bs2 qvs2 qts2 qcs2) =
  State
    { _vals    = Map.unionWith Env.mergeValues vs1 vs2
    , _types   = Map.unionWith Env.mergeTypes  ts1 ts2
    , _ctors   = Map.unionWith Env.mergeTypes  cs1 cs2
    , _binops  = Map.union bs1 bs2
    , _q_vals  = Map.unionWith (Map.unionWith Env.mergeValues) qvs1 qvs2
    , _q_types = Map.unionWith (Map.unionWith Env.mergeTypes ) qts1 qts2
    , _q_ctors = Map.unionWith (Map.unionWith Env.mergeTypes ) qcs1 qcs2
    }



-- TO SAFE IMPORTS


toSafeImports :: ModuleName.Canonical -> [Src.Import] -> [Src.Import]
toSafeImports (ModuleName.Canonical pkg _) imports =
  if Pkg.isKernel pkg
  then filter isNormal imports
  else imports


isNormal :: Src.Import -> Bool
isNormal (Src.Import (A.At _ name) maybeAlias _) =
  if Module.isKernel name
  then
    case maybeAlias of
      Nothing -> False
      Just _  -> $(Crash.crash 'isNormal) "kernel imports cannot use `as`"
  else
    True



-- INITIALIZE IMPORTS


init :: Map.Map Module.Name I.Interface -> Src.Import -> Result State
init ifaces (Src.Import (A.At _ name) maybeAlias exposing) =
  let
    (I.Interface pkg values types binops) = $(Map.require 'init) name ifaces Module.toChars
    !prefix = determinePrefix name maybeAlias
    !home   = ModuleName.Canonical pkg name
    !nodes  = Map.mapMaybeWithKey (toTypeAndCtors home) types

    !vs = Map.map (toForeignValue home) values
    !ts = Map.map toTypeInfo nodes
    !cs = Map.foldr (Map.union . getCtors) Map.empty nodes
    !bs = Map.map (toBinop home) binops

    !qvs = Map.singleton prefix vs
    !qts = Map.singleton prefix ts
    !qcs = Map.singleton prefix cs
  in
  case exposing of
    Src.Open ->
      pure $ State vs ts cs bs qvs qts qcs

    Src.Explicit exposeds ->
      do  (Exposing uppers lowers ops) <- checkExposing exposeds
          (\v (t,c) b -> State v t c b qvs qts qcs)
            <$> toExposedValues home lowers vs
            <*> toExposedTypes  home uppers nodes
            <*> toExposedBinops home ops bs


determinePrefix :: Module.Name -> Maybe Module.Prefix -> Module.Prefix
determinePrefix home maybeAlias =
  case maybeAlias of
    Just prefix -> prefix
    Nothing     -> Module.toPrefix home


toForeignValue :: ModuleName.Canonical -> I.Annotation -> Env.ForeignValue
toForeignValue home anno =
  Env.ForeignValue home anno


toTypeInfo :: TypeAndCtors -> Type UnionInfo AliasInfo
toTypeInfo tcs =
  case tcs of
    UnionAndCtors s i _ -> Env.Union s i
    AliasAndCtors s i _ -> Env.Alias s i


toBinop :: ModuleName.Canonical -> I.Binop -> Env.Binop
toBinop h (I.Binop n t a p) =
  Env.Binop h n t a p



-- TO EXPOSED VALUES


toExposedValues :: ModuleName.Canonical -> Map.Map N.Name A.Region -> Map.Map N.Name Env.ForeignValue -> Result (Map.Map N.Name Env.ForeignValue)
toExposedValues home lowers values =
    Map.mergeA (Map.traverseMissing notFound) Map.dropMissing (Map.zipWithAMatched found) lowers values
  where
    notFound name region =
      throw $ E.ImportExposedValueNotFound region home name (Map.keys values)

    found _ _ node =
      return node



-- TO EXPOSED BINOPS


toExposedBinops :: ModuleName.Canonical -> Map.Map Op.Name A.Region -> Map.Map Op.Name Env.Binop -> Result (Map.Map Op.Name Env.Binop)
toExposedBinops home ops binops =
    Map.mergeA (Map.traverseMissing notFound) Map.dropMissing (Map.zipWithAMatched found) ops binops
  where
    notFound name region =
      throw $ E.ImportExposedBinopNotFound region home name (Map.keys binops)

    found _ _ binop =
      return binop



-- TO EXPOSED TYPES


toExposedTypes :: ModuleName.Canonical -> Map.Map T.Name (A.Region, Src.Privacy) -> Map.Map T.Name TypeAndCtors -> Result (Map.Map T.Name (Type UnionInfo AliasInfo), Map.Map N.Name (Type UnionCtor AliasCtor))
toExposedTypes home uppers types =
  do  exposeds <- Map.mergeA (Map.traverseMissing notFound) Map.dropMissing (Map.zipWithAMatched found) uppers types
      return
        ( Map.map toTypeInfo exposeds
        , Map.foldr (Map.union . getCtors) Map.empty exposeds
        )
  where
    notFound name info =
      throw $ importExposedTypeNotFound home name info types

    found name (_, privacy) typeAndCtors =
      case typeAndCtors of
        UnionAndCtors s i _ ->
          case privacy of
            Src.Private  -> return $ UnionAndCtors s i Map.empty
            Src.Public _ -> return typeAndCtors

        AliasAndCtors _ _ _ ->
          case privacy of
            Src.Private  -> return typeAndCtors
            Src.Public r -> throw $ E.ImportOpenAlias r name


importExposedTypeNotFound :: ModuleName.Canonical -> T.Name -> (A.Region, Src.Privacy) -> Map.Map T.Name TypeAndCtors -> E.Error
importExposedTypeNotFound home exposedName (region, privacy) types =
  case privacy of
    Src.Public _ ->
      E.ImportExposedTypeNotFound region home exposedName (Map.keys types)

    Src.Private ->
      case Map.foldrWithKey checkType [] types of
        []     -> E.ImportExposedTypeNotFound region home exposedName (Map.keys types)
        tipe:_ -> E.ImportCtorByName region name tipe
  where
    name = T.nameToName exposedName

    checkType typeName typeAndCtors matches =
      case typeAndCtors of
        UnionAndCtors _ _ cs -> Map.foldrWithKey (checkCtor typeName) matches cs
        AliasAndCtors _ _ _  -> matches

    checkCtor typeName ctorName _ matches =
      if name == ctorName
      then typeName : matches
      else matches



-- TO TYPE AND CTORS
--
-- The environment needs nodes for qualified and unqualified names. These nodes
-- are a bit more complicated to allocate for types and constructors, so this
-- code takes care of it in a generic way that can be used for both cases.
--
-- NOTE: maybe add information so that a "type exists but is not exposed" error
-- can be given when importing local modules. Such an error should not be given
-- when importing from a package though.


data TypeAndCtors
  = UnionAndCtors ModuleName.Canonical Env.UnionInfo (Map.Map N.Name (Type UnionCtor AliasCtor))
  | AliasAndCtors ModuleName.Canonical Env.AliasInfo (Maybe  (N.Name, Type UnionCtor AliasCtor))


toTypeAndCtors :: ModuleName.Canonical -> T.Name -> I.TypeDecl -> Maybe TypeAndCtors
toTypeAndCtors home name decl =
  case decl of
    I.UnionOpen    u -> Just $ toUnionAndCtors home name u
    I.UnionClosed  u -> Just $ toUnionWithoutCtors u
    I.UnionPrivate _ -> Nothing
    I.AliasPublic  a -> Just $ toAliasAndCtors home name a
    I.AliasPrivate _ -> Nothing
  where
    toUnionWithoutCtors (I.Union m xs _) =
      UnionAndCtors home (Env.UnionInfo m (toArity xs)) Map.empty


getCtors :: TypeAndCtors -> Map.Map N.Name (Type UnionCtor AliasCtor)
getCtors typeAndCtors =
  case typeAndCtors of
    UnionAndCtors _ _ ctors        -> ctors
    AliasAndCtors _ _ (Just (n,c)) -> Map.singleton n c
    AliasAndCtors _ _ Nothing      -> Map.empty



-- TO ALIAS AND CTORS


toAliasAndCtors :: ModuleName.Canonical -> T.Name -> I.Alias -> TypeAndCtors
toAliasAndCtors home name (I.Alias mask vars aliased@(I.Aliased tipe)) =
  AliasAndCtors home (Env.AliasInfo mask (toArity vars) aliased) $
    case tipe of
      I.TLambda _ _ _    -> Nothing
      I.TVar _           -> Nothing
      I.TAlias _ _ _ _ _ -> Nothing
      I.TType _ _ _ _    -> Nothing
      I.TUnit            -> Nothing
      I.TPair _ _        -> Nothing
      I.TTriple _ _ _    -> Nothing
      I.TEmpty           -> Nothing
      I.TRecord fs       -> Just (toCtor fs)
      I.TExtRec _ _      -> Nothing
  where
    toCtor fields =
      let
        ((n,t),(ns,ts)) = fmap List.unzip $ F.flatten fields
        result          = I.TAlias home name mask (T.imap I.TVar vars) aliased
        anno            = I.Forall vars $ List.snoc1 (I.TLambda t) ts result
      in
      ( T.nameToName name
      , Env.Alias home (Env.Record anno n ns)
      )


toArity :: A.Array T.Var -> Env.Arity
toArity vars =
  Env.Arity (A.size vars)



-- TO UNION AND CTORS


toUnionAndCtors :: ModuleName.Canonical -> T.Name -> I.Union -> TypeAndCtors
toUnionAndCtors h n (I.Union mask vars variants) =
  UnionAndCtors h (Env.UnionInfo mask (toArity vars)) $
    let
      result = I.TType h n mask (T.imap I.TVar vars)
    in
    case variants of
      I.Variants table ->
        let
          ptype = Can.PType h n mask vars (vNits table)
        in
        Tag.toDict table $ \tag (I.Variant name args) ->
          ( name
          , Env.Union h $ Env.Variant (toAnno vars args result) ptype tag (List.length args) args
          )

      I.Enum table ->
        let
          anno  = I.Forall vars result
          ptype = Can.PType h n mask vars (cNits table)
        in
        Tag.toDict table $ \tag name ->
          ( name
          , Env.Union h $ Env.Enum anno ptype tag
          )

      I.Vector name a b cs ->
        let
          len   = 2 + List.length cs
          anno  = I.Forall vars (I.TLambda a b (cs ++ [result]))
          ptype = Can.PType h n mask vars (Nit.vector len name)
          node  = Env.Union h (Env.Vector anno ptype len a b cs)
        in
        Map.singleton name node

      I.Box name arg ->
        let
          anno  = I.Forall vars (I.TLambda arg result [])
          ptype = Can.PType h n mask vars (Nit.box name)
          node  = Env.Union h (Env.Box anno ptype arg)
        in
        Map.singleton name node

      I.Void name ->
        let
          anno  = I.Forall vars result
          ptype = Can.PType h n mask vars (Nit.void name)
          node  = Env.Union h (Env.Void anno ptype)
        in
        Map.singleton name node


vNits :: Tag.Table I.Variant -> Nit.Variants
vNits table =
  Tag.unpack table Nit.Variants_ $ \t (I.Variant n xs) ->
    Nit.Variant n t (List.length xs)


cNits :: Tag.Table N.Name -> Nit.Variants
cNits table =
  Tag.unpack table Nit.Variants_ $ \t n ->
    Nit.Variant n t 0


toAnno :: A.Array T.Var -> [I.Type] -> I.Type -> I.Annotation
toAnno vars args result =
  I.Forall vars $
    case args of
      []   -> result
      t:ts -> List.snoc1 (I.TLambda t) ts result



-- CHECK EXPOSING
--
-- TODO actually use Dups and give errors! This is not checked in 0.19.1 and
-- lower, so it makes sense to enable this in a major release to avoid breaking
-- existing packages that may have this mistake.


data Exposing =
  Exposing
    { _uppers :: Map.Map T.Name (A.Region, Src.Privacy)
    , _lowers :: Map.Map N.Name A.Region
    , _ops    :: Map.Map Op.Name A.Region
    }


checkExposing :: [Src.Exposed] -> Result Exposing
checkExposing =
    go Map.empty Map.empty Map.empty
  where
    go uppers lowers ops exposeds =
      case exposeds of
        [] ->
          pure $ Exposing uppers lowers ops

        e:es ->
          case e of
            Src.Upper (A.At r n) p -> go (Map.insertWith mergePrivacy n (r,p) uppers) lowers ops es
            Src.Lower (A.At r n)   -> go uppers (Map.insert n r lowers) ops es
            Src.Operator r n       -> go uppers lowers (Map.insert n r ops) es


mergePrivacy :: (A.Region, Src.Privacy) -> (A.Region, Src.Privacy) -> (A.Region, Src.Privacy)
mergePrivacy p1@(_,privacy) p2 =
  case privacy of
    Src.Public _ -> p1
    Src.Private  -> p2



-- RESULT


throw :: E.Error -> Result a
throw x =
  Result $ \_ err -> err (OOM.one x)


instance Functor Result where
  {-# INLINE fmap #-}
  fmap func (Result k) =
    Result $ \ok err -> k (ok . func) err


instance Applicative Result where
  {-# INLINE pure #-}
  pure a =
    Result $ \ok _ -> ok a

  {-# INLINE (<*>) #-}
  (<*>) (Result kF) (Result kA) =
    Result $ \ok err ->
      let
        okF func = kA (ok . func) err
        errF xs  = kA (\_ -> err xs) (\ys -> err (OOM.more xs ys))
      in
      kF okF errF


instance Monad Result where
  {-# INLINE (>>=) #-}
  (>>=) (Result kA) callback =
    Result $ \ok err ->
      kA (\a -> case callback a of { Result kB -> kB ok err }) err
