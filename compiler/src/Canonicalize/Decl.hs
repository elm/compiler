{-# LANGUAGE BangPatterns, GADTs, Rank2Types, TemplateHaskell #-}
module Canonicalize.Decl
  ( Result(..)
  , canonicalize
  )
  where


import Data.Foldable (traverse_)
import qualified Data.Dups as Dups
import qualified Data.List as List
import qualified Data.List.Utils as List
import qualified Data.Map.Strict as Map
import qualified Data.OneOrMore as OOM

import qualified Crash
import qualified Graph

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Prim.Name as N
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Expression as Expr
import qualified Canonicalize.Pattern as Pattern
import qualified Canonicalize.Type as Type
import qualified Elm.Interface as I
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as E
import qualified Reporting.Warning as W



-- CANONICALIZE


newtype Result a =
  Result
  (
    forall r.
      [W.Warning]
      -> ([W.Warning] -> a -> IO r)
      -> ([W.Warning] -> OOM.OneOrMore E.Problem -> IO r)
      -> IO r
  )


canonicalize :: Env.Env -> [Src.Value] -> Result Can.Decls
canonicalize env values =
  do  nodes <- traverse (canDecl env) values
      decls <- traverse checkRecursion (Graph.toSCC nodes)
      return $ Can.Decls (Graph.Sorted decls)



-- CANONICALIZE DECL
--
-- Keep the dependencies because, it is necessary to check cycles to see if
-- they remain cycles when excluding Delayed uses.
--
-- TODO disallow constrained types that *only* appear in the return type and
-- disallow number/comparable constraints in record extensions. And do this for
-- annotations in let-expressions as well.


data Def =
  Def
    { _def :: Can.Def
    , _deps :: Map.Map N.Name Expr.Usage
    }


canDecl :: Env.Env -> Src.Value -> Result (Graph.Node N.Name Def)
canDecl env (Src.Value name@(A.At _ n) patterns expr maybeType) =
  case maybeType of
    Just tipe ->
      Type.canonicalize env tipe (go . Can.toIAnnotation) throws

    Nothing ->
      case patterns of
        [] ->
          bind env name
            (\_    -> pure ())
            (\env2 -> Expr.canonicalize env2 Expr.Direct expr)
            (\() e -> Can.DValue_ name e)

        arg:args ->
          bind env name
            (\env1     -> List.traverse1 (Pattern.canonicalize env1) arg args)
            (\env2     -> Expr.canonicalize env2 Expr.Delayed expr)
            (\(p,ps) e -> Can.DFunc_ name p ps e)
  where
    go anno@(I.Forall _ tipe) =
      case patterns of
        [] ->
          bind env name
            (\_    -> pure ())
            (\env2 -> Expr.canonicalize env2 Expr.Direct expr)
            (\() e -> Can.DValue name e tipe anno)

        arg:args ->
          bind env name
            (\env1       -> Pattern.canTypedArgs env1 n arg args tipe)
            (\env2       -> Expr.canonicalize env2 Expr.Delayed expr)
            (\(p,ps,t) e -> Can.DFunc name p ps e t anno)



-- BIND


bind
  :: Env.Env
  -> A.Located N.Name
  -> (Env.Env -> Pattern.Result pattern)
  -> (Env.Env -> Expr.Result expr)
  -> (pattern -> expr -> Can.Def)
  -> Result (Graph.Node N.Name Def)
bind env (A.At _ name) toPattern toExpr toDef =
  Result $ \warnings ok err ->
    let
      ok1 dups args =
        case Dups.detect (\_ r _ -> r) (E.DuplicatePattern (E.DPFuncArgs name)) dups of
          Dups.Detector k ->
            k ok2 (err warnings)
        where
          ok2 bindings =
            case toExpr =<< Expr.addLocals bindings env of
              Expr.Result k ->
                k Map.empty Map.empty warnings ok3 err
            where
              ok3 locals tops warnings3 expr =
                let
                  !warns = addUnused bindings locals warnings3
                  !def   = toDef args expr
                  !edges = Map.keys tops
                in
                ok warns (Graph.Node name (Def def tops) edges)
    in
    case toPattern env of
      Pattern.Result k ->
        k Dups.none ok1 (err warnings)


addUnused :: Map.Map N.Name A.Region -> Map.Map N.Name () -> [W.Warning] -> [W.Warning]
addUnused bindings locals warnings =
  Crash.assert (Map.null (Map.difference locals bindings)) $
    if Map.size bindings == Map.size locals
    then warnings
    else Map.foldrWithKey cons warnings (Map.difference bindings locals)
  where
    cons n r ws = W.UnusedVar r n : ws



--------------------------------------------------------------------------------
-- CHECK RECURSION -------------------------------------------------------------
--------------------------------------------------------------------------------
--
-- There are two phases of cycle detection:
--
-- 1. Edges for Direct/Delayed usage. This gets declarations properly grouped for
--    type inference.
--
-- 2. Edges for Direct usage ONLY. This detects NON-TERMINATING recursive values.
--    If the recursion is delayed by a function, it is allowed.
--
--------------------------------------------------------------------------------



-- CHECK RECURSION


checkRecursion :: Graph.SCC N.Name Def -> Result Can.Decl
checkRecursion scc =
  case scc of
    Graph.Acyclic (Graph.Node _ (Def def _) _) ->
      pure $ Can.Decl def

    Graph.Cyclic component ->
      Graph.withComponent component $ \n ns ->
        let
          node  = toDirect n
          nodes = List.map toDirect ns
        in
        case traverse_ toAcyclic $ Graph.toSCC (node:nodes) of
          Right () -> pure $ Can.DeclRec (Graph._value node) (List.map Graph._value nodes)
          Left c   -> throw $ withMinimalCycle c id E.RecursiveDecl
  where
    toAcyclic scc_ =
      case scc_ of
        Graph.Acyclic _ -> Right ()
        Graph.Cyclic  c -> Left c

    toDirect (Graph.Node key (Def def deps) _) =
      Graph.Node key def (Map.foldrWithKey cons [] deps)

    cons key uses keys =
      case uses of
        Expr.Direct  -> key:keys
        Expr.Delayed -> keys



-- WITH MINIMAL CYCLE


withMinimalCycle :: Graph.Component N.Name v -> (v -> Can.Def) -> (A.Located N.Name -> Graph.MinimalCycle N.Name -> r) -> r
withMinimalCycle component toDef cont =
    Graph.withMinimalCycle selector component Graph._key cont
  where
    selector =
      Graph.RootSelector $ \node nodes ->
        let
          first = fst $ List.sortBy1 (A.toRegion . toLName) node nodes
        in
        ( Graph._key first, toLName first )

    toLName (Graph.Node _ v _) =
      toDefName $ toDef v



--------------------------------------------------------------------------------
-- HELPERS ---------------------------------------------------------------------
--------------------------------------------------------------------------------



-- TO DEF NAME


toDefName :: Can.Def -> A.Located N.Name
toDefName def =
  case def of
    Can.DValue_ n     _     -> n
    Can.DValue  n     _ _ _ -> n
    Can.DFunc_  n _ _ _     -> n
    Can.DFunc   n _ _ _ _ _ -> n



-- RESULT HELPERS


throw :: E.Problem -> Result a
throw x =
  Result $ \warns _ err ->
    err warns (OOM.one x)


throws :: OOM.OneOrMore E.Problem -> Result a
throws xs =
  Result $ \warns _ err ->
    err warns xs


instance Functor Result where
  {-# INLINE fmap #-}
  fmap f (Result k) =
    Result $ \warns ok err ->
      k warns (\w a -> ok w (f a)) err


instance Applicative Result where
  {-# INLINE pure #-}
  pure a =
    Result $ \warns ok _ ->
      ok warns a

  {-# INLINE (<*>) #-}
  (<*>) (Result kF) (Result kA) =
    Result $ \warns ok err ->
      let
        okF  ws f  = kA ws (\w a -> ok w (f a)) err
        errF ws xs = kA ws (\w _ -> err w xs) (\w ys -> err w (OOM.more xs ys))
      in
      kF warns okF errF


instance Monad Result where
  {-# INLINE (>>=) #-}
  (>>=) (Result kA) callback =
    Result $ \warns ok err ->
      let
        okA ws a =
          case callback a of
            Result kB -> kB ws ok err
      in
      kA warns okA err
