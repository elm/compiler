{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, PatternGuards, QuasiQuotes,
Rank2Types, TemplateHaskell, UnboxedTuples
#-}
module Optimize.Module
  ( Result(..)
  , optimize
  )
  where


import Prelude hiding (cycle)
import Data.Foldable (foldlM)
import qualified Data.List as List hiding (foldr1)
import qualified Data.List.Utils as List
import qualified Data.Map as Map
import qualified Data.Map.Utils as Map
import qualified Data.NonEmptyMap as NEM
import qualified Data.OneOrMore as OOM
import qualified Data.Set as Set
import GHC.Prim
import GHC.ST (ST(ST), runST)

import qualified Array as A
import qualified Graph
import qualified String as S

import qualified AST.Canonical as Can
import qualified AST.Typed as Tpd
import qualified AST.Optimized as Opt
import qualified AST.Prim.Fields as F
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.Tag as Tag
import qualified AST.Prim.TypeName as T
import qualified Canonicalize.Effects as Effects
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Optimize.Expression as Expr
import qualified Optimize.Names as Names
import qualified Optimize.Port as Port
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Main as E
import qualified Reporting.Warning as W



-- OPTIMIZE


newtype Result a =
  Result
  (
    forall r.
      [W.Warning]
      -> ([W.Warning] -> a -> r)
      -> ([W.Warning] -> OOM.OneOrMore E.Error -> r)
      -> r
  )


type Annotations =
  Map.Map N.Name I.Annotation


optimize :: Annotations -> Tpd.Module -> Result Opt.LocalGraph
optimize annotations (Tpd.Module home _ _ decls types _ effects) =
  optDecls home annotations decls $
    Opt.LocalGraph Nothing (Map.union tNodes eNodes) (Map.union tCounts eCounts)
  where
    (Locals tNodes tCounts) = optTypes home types
    (Locals eNodes eCounts) = optEffects home effects


data Locals =
  Locals
    { _nodes :: Map.Map Opt.Global Opt.Node
    , _counts :: Map.Map N.Name Int
    }


addLocal :: Opt.Global -> Opt.Node -> Map.Map N.Name Int -> Locals -> Locals
addLocal name node cs (Locals nodes counts) =
  Locals (Map.insert name node nodes) (Map.unionWith (+) cs counts)


addLocals :: Map.Map Opt.Global Opt.Node -> Map.Map N.Name Int -> Locals -> Locals
addLocals ns cs (Locals nodes counts) =
  Locals (Map.union ns nodes) (Map.unionWith (+) cs counts)


addNodes :: Map.Map Opt.Global Opt.Node -> Locals -> Locals
addNodes ns (Locals nodes counts) =
  Locals (Map.union ns nodes) counts



-- TYPES


optTypes :: ModuleName.Canonical -> Map.Map T.Name Can.TypeDecl -> Locals
optTypes home types =
    Map.foldrWithKey add (Locals Map.empty Map.empty) types
  where
    add name tipe locals =
      case tipe of
        Can.Union u -> addNodes (optUnion home u) locals
        Can.Alias a -> optAlias home name a (\ns cs -> addLocals ns cs locals) locals


optUnion :: ModuleName.Canonical -> I.Union -> Map.Map Opt.Global Opt.Node
optUnion h (I.Union _ _ variants) =
  case variants of
    I.Variants     vs -> Tag.toDict vs goV
    I.Enum         es -> Tag.toDict es goE
    I.Vector n _ _ cs -> Map.singleton (Opt.Global h n) (Opt.Vector (2 + List.length cs))
    I.Box    n _      -> Map.singleton (Opt.Global h n) Opt.Box
    I.Void   n        -> Map.singleton (Opt.Global h n) Opt.Void
  where
    goV t (I.Variant n ts) = (Opt.Global h n, Opt.Variant t (List.length ts))
    goE t            n     = (Opt.Global h n, Opt.Enum t)


optAlias :: ModuleName.Canonical -> T.Name -> I.Alias -> (Map.Map Opt.Global Opt.Node -> Map.Map N.Name Int -> r) -> r -> r
optAlias home name (I.Alias _ _ (I.Aliased tipe)) yesRecord noRecord =
  case tipe of
    I.TRecord fields ->
      let
        k = Opt.Global home (T.nameToName name)
        v = Opt.Define (function fields) Set.empty
      in
      yesRecord (Map.singleton k v) (toOnes fields)

    _ ->
      noRecord
  where
    toOnes fields =
      Map.map (const 1) (NEM.toDict (F.toNEM fields))

    function fields =
      uncurry Opt.Function (F.flattenKeys fields) $ Opt.Record $
        NEM.mapWithKey (\f _ -> Opt.VarLocal f) (F.toNEM fields)



-- EFFECTS


optEffects :: ModuleName.Canonical -> Can.Style -> Locals
optEffects home effects =
  case effects of
    Can.Normal ->
      Locals Map.empty Map.empty

    Can.Ports ports ->
      Map.foldrWithKey add (Locals Map.empty Map.empty) ports

    Can.Effect _ _ _ manager ->
      let
        fx    = Opt.Global home [N.ascii|$fx$|]
        cmd   = Opt.Global home [N.ascii|command|]
        sub   = Opt.Global home [N.ascii|subscription|]
        link  = Opt.Link fx
        nodes =
          case manager of
            Can.Cmd _   -> Map.insert cmd link                       $ Map.singleton fx (Opt.Manager Opt.Cmd)
            Can.Sub   _ ->                       Map.insert sub link $ Map.singleton fx (Opt.Manager Opt.Sub)
            Can.Fx  _ _ -> Map.insert cmd link $ Map.insert sub link $ Map.singleton fx (Opt.Manager Opt.Fx)
      in
      Locals nodes Map.empty

  where
    add name port locals =
      case port of
        Can.Incoming _ t _ ->
          Names.with (Port.toDecoder t) $ \deps counts decoder ->
            addLocal (Opt.Global home name) (Opt.PortIncoming decoder deps) counts locals

        Can.Outgoing _ t _ ->
          Names.with (Port.toEncoder t) $ \deps counts encoder ->
            addLocal (Opt.Global home name) (Opt.PortOutgoing encoder deps) counts locals



-- DECLS


optDecls :: ModuleName.Canonical -> Annotations -> Can.Decls -> Opt.LocalGraph -> Result Opt.LocalGraph
optDecls home annotations (Can.Decls (Graph.Sorted decls)) graph0 =
    foldlM optDecl graph0 decls
  where
    optDecl graph decl =
      case decl of
        Can.Decl    d    -> optDef home annotations d graph
        Can.DeclRec d ds ->
          case findMain d ds of
            Nothing -> pure $ optRecDefs home d ds graph
            Just r  -> throw $ E.BadCycle r $ Graph.MinimalCycle (go d) (map go ds)

    go def =
      A.toValue (defToName def)


findMain :: Can.Def -> [Can.Def] -> Maybe A.Region
findMain def defs =
  let (A.At region name) = defToName def in
  if name == N.main
  then Just region
  else
    case defs of
      []   -> Nothing
      d:ds -> findMain d ds


defToName :: Can.Def -> A.Located N.Name
defToName def =
  case def of
    Can.DValue_ n     _     -> n
    Can.DValue  n     _ _ _ -> n
    Can.DFunc_  n _ _ _     -> n
    Can.DFunc   n _ _ _ _ _ -> n



-- ADD DEFS


optDef :: ModuleName.Canonical -> Annotations -> Can.Def -> Opt.LocalGraph -> Result Opt.LocalGraph
optDef home annotations def graph0 =
  case def of
    Can.DValue_ n      e     -> warn n $ \t -> optValue n e t graph0
    Can.DValue  n      e _ t ->                optValue n e t graph0
    Can.DFunc_  n x xs e     -> warn n $ \t -> optFunc n x xs e t graph0
    Can.DFunc   n x xs e _ t ->                optFunc n (Can._arg x) (map Can._arg xs) e t graph0
  where
    optFunc (A.At r name) x xs body t graph =
      if name == N.main
      then throw (E.BadType r t)
      else
        let
          (deps, fields, odef) =
            Names.run $
              do  (y, ys, destructors) <- Expr.destructArgs x xs
                  obody <- Expr.optimize Set.empty body
                  pure $ Opt.Function y ys $ foldr Opt.Destruct obody destructors
        in
        pure $ add home name (Opt.Define odef deps) fields graph

    optValue (A.At r name) body t graph =
      do  namer <- if name == N.main then toMain addMain r t else pure (pure id)
          let (deps, fields, (adder, odef)) = Names.run $ liftA2 (,) namer $ Expr.optimize Set.empty body
          pure $ add home name (Opt.Define odef deps) fields (adder graph)

    addMain main graph =
      graph { Opt._l_main = Just main }

    add h n node cs (Opt.LocalGraph main nodes counts) =
      Opt.LocalGraph
        main
        (Map.insert (Opt.Global h n) node nodes)
        (Map.unionWith (+) cs counts)

    toMain func r tipe@(I.Forall xs t) =
      case I.deepDealias t of
        I.TType h n _ _ | h == ModuleName.virtualDom && n == T.node ->
          pure $ func <$> Names.registerKernel Module.kernel_vdom Opt.Static

        I.TType h n _ args | h == ModuleName.platform && n == T.program, [flags, _, msg] <- A.toList args ->
          case Effects.checkPayload xs flags of
            Right () -> pure $ func . Opt.Dynamic xs msg <$> Port.toFlagsDecoder flags
            Left x   -> throw $ E.BadFlags r x

        _ ->
          throw $ E.BadType r tipe

    warn n@(A.At _ name) cont =
      Result $ \ws ok err ->
        let
          t = $(Map.require 'optDef) name annotations N.toChars
          w = W.MissingTypeAnnotation n t
        in
        case cont t of
          Result k -> k (w:ws) ok err



-- ADD RECURSIVE DEFS


data State =
  State
    { _values :: [(N.Name, Opt.Expr)]
    , _functions :: [Opt.Def]
    }


optRecDefs :: ModuleName.Canonical -> Can.Def -> [Can.Def] -> Opt.LocalGraph -> Opt.LocalGraph
optRecDefs home def defs (Opt.LocalGraph main nodes fieldCounts) =
  let
    (n,ns) = List.reverse1 (toName def) (map toName defs)
    cycleName = Opt.Global home (toCycleName n ns)
    cycle = List.foldr1 addValueName Set.empty def defs
    links = List.foldr1 (addLink home (Opt.Link cycleName)) Map.empty n ns

    (deps, fields, State values funcs) =
      Names.run $ foldlM (addRecDef cycle) (State [] []) defs
  in
  Opt.LocalGraph
    main
    (Map.insert cycleName (Opt.Cycle n ns values funcs deps) (Map.union links nodes))
    (Map.unionWith (+) fields fieldCounts)


toName :: Can.Def -> N.Name
toName def =
  case def of
    Can.DValue_ (A.At _ n)     _     -> n
    Can.DValue  (A.At _ n)     _ _ _ -> n
    Can.DFunc_  (A.At _ n) _ _ _     -> n
    Can.DFunc   (A.At _ n) _ _ _ _ _ -> n


addValueName :: Can.Def -> Set.Set N.Name -> Set.Set N.Name
addValueName def names =
  case def of
    Can.DValue_ (A.At _ n)     _     -> Set.insert n names
    Can.DValue  (A.At _ n)     _ _ _ -> Set.insert n names
    Can.DFunc_  _          _ _ _     -> names
    Can.DFunc   _          _ _ _ _ _ -> names


addLink :: ModuleName.Canonical -> Opt.Node -> N.Name -> Map.Map Opt.Global Opt.Node -> Map.Map Opt.Global Opt.Node
addLink home link name links =
  Map.insert (Opt.Global home name) link links



-- ADD RECURSIVE DEFS


addRecDef :: Set.Set N.Name -> State -> Can.Def -> Names.Tracker State
addRecDef cycle state def =
  case def of
    Can.DValue_ n      e     -> goV n e state
    Can.DValue  n      e _ _ -> goV n e state
    Can.DFunc_  n x xs e     -> goF n x xs e state
    Can.DFunc   n x xs e _ _ -> goF n (Can._arg x) (List.map Can._arg xs) e state
  where
    goV (A.At _ name) body (State values funcs) =
      do  obody <- Expr.optimize cycle body
          pure $ State ((name, obody) : values) funcs

    goF (A.At _ name) x xs body (State values funcs) =
      do  odef <- Expr.optSelfRec cycle name x xs body
          pure $ State values (odef : funcs)



-- TO CYCLE NAME
--
-- Creating a unique name by combining all the subnames can create names
-- longer than 256 bytes relatively easily. So instead, the first given name
-- (e.g. foo) is prefixed chars that are valid in JS but not Elm (e.g. _C$foo)
--
-- This should be a unique name since top-level cycles cannot have overlapping
-- declarations, so the important thing is to make the cycle name distinct from
-- normal names.


toCycleName :: N.Name -> [N.Name] -> N.Name
toCycleName name _ =
  runST $ ST $ \s0 ->
    let
      !(S.String ba) = N.toString name
      len = sizeofByteArray# ba
    in
    case newByteArray# (3# +# len)                s0 of { (# s1, mba #) ->
    case writeWord8Array# mba 0# 0x5F#Word8 {-_-} s1 of {    s2         ->
    case writeWord8Array# mba 1# 0x43#Word8 {-C-} s2 of {    s3         ->
    case writeWord8Array# mba 2# 0x24#Word8 {-$-} s3 of {    s4         ->
    case copyByteArray# ba 0# mba 3# len          s4 of {    s5         ->
    case unsafeFreezeByteArray# mba               s5 of { (# s6, ba' #) ->
      (# s6, N.fromString (S.String ba') #)
    }}}}}}



-- RESULT HELPERS


throw :: E.Error -> Result a
throw x =
  Result $ \warns _ err ->
    err warns (OOM.one x)


instance Functor Result where
  {-# INLINE fmap #-}
  fmap func (Result k) =
    Result $ \warns ok err ->
      let
        ok' w arg = ok w (func arg)
      in
      k warns ok' err


instance Applicative Result where
  {-# INLINE pure #-}
  pure a =
    Result $ \w ok _ -> ok w a

  {-# INLINE (<*>) #-}
  (<*>) (Result kF) (Result kA) =
    Result $ \warns ok err ->
      let
        okF ws func =
          let
            okA w arg = ok w (func arg)
          in
          kA ws okA err

        errF ws xs =
          let
            okA  w _  = err w xs
            errA w ys = err w (OOM.more xs ys)
          in
          kA ws okA errA
      in
      kF warns okF errF


instance Monad Result where
  {-# INLINE (>>=) #-}
  (>>=) (Result kA) callback =
    Result $ \warns ok err ->
      let
        okA w a = case callback a of { Result kB -> kB w ok err }
      in
      kA warns okA err
