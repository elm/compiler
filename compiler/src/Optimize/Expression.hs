{-# LANGUAGE OverloadedStrings #-}
module Optimize.Expression
  ( optimize
  , destructArgs
  , optSelfRec
  )
  where


import Prelude hiding (cycle)
import Control.Monad (foldM)
import qualified Data.Map as Map
import qualified Data.NonEmptyMap as NEM
import qualified Data.Set as Set

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Prim.Fields as F
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.Shader as Shader
import qualified Data.Index as Index
import qualified Elm.ModuleName as ModuleName
import qualified Optimize.Case as Case
import qualified Optimize.Names as Names
import qualified Reporting.Annotation as A



-- OPTIMIZE


type Cycle =
  Set.Set N.Name


optimize :: Cycle -> Can.Expr -> Names.Tracker Opt.Expr
optimize cycle =
    go
  where
    go (A.At region expression) =
      case expression of
        Can.VarLocal n -> pure (Opt.VarLocal n)

        Can.VarTopLevel h n ->
          if Set.member n cycle
          then pure (Opt.VarCycle h n)
          else Names.registerGlobal h n

        Can.VarKernel     h n   -> Names.registerKernel h (Opt.VarKernel h n)
        Can.VarForeign    h n _ -> Names.registerGlobal h n
        Can.VarDebug      h n _ -> Names.registerDebug n h region
        Can.VarOperator _ h n _ -> Names.registerGlobal h n
        Can.CRecord  h n _ _ _  -> Names.registerRecord h n
        Can.CVariant h n _ _ _  -> Names.registerVariant h n
        Can.CEnum    h n _ t    -> Names.registerEnum h n t
        Can.CVector  h n _ _    -> Names.registerVector h n
        Can.CBox     h n _      -> Names.registerBox h n
        Can.CVoid    h n _      -> Names.registerVoid h n
        Can.Bool b              -> pure (Opt.Bool b)
        Can.Chr c               -> Names.registerKernel Module.kernel_utils (Opt.Chr c)
        Can.Str s               -> pure (Opt.Str s)
        Can.Int i               -> pure (Opt.Int i)
        Can.Float f             -> pure (Opt.Float f)
        Can.List es             -> Names.registerKernel Module.kernel_list Opt.List <*> traverse go es
        Can.Negate e            -> Opt.call1 <$> Names.registerGlobal ModuleName.basics N.negate <*> go e
        Can.Binop _ h n _ x y   -> Opt.call2 <$> Names.registerGlobal h n <*> go x <*> go y

        Can.Lambda x xs body ->
          do  (y, ys, destructors) <- destructArgs x xs
              obody <- go body
              pure $ Opt.Function y ys (foldr Opt.Destruct obody destructors)

        Can.Call f x xs   -> Opt.Call <$> go f <*> go x <*> traverse go xs
        Can.If bs f       -> Opt.If <$> traverse goThen bs <*> go f
        Can.LetValue v b  -> optLetValue cycle v =<< go b
        Can.LetFunc f b   -> optLetFunc  cycle f =<< go b
        Can.LetRec f [] b -> Opt.Let <$> optSelfRecursive cycle f <*> go b
        Can.LetRec f fs b ->
          do  obody <- go b
              foldM (\bod func -> optLetFunc cycle func bod) obody (f:fs)

        Can.LetDestruct pattern expr body ->
          do  (name, destructs) <- destruct pattern
              oexpr <- go expr
              obody <- go body
              pure $ Opt.Let (Opt.Def name oexpr) (foldr Opt.Destruct obody destructs)

        Can.Case expr branch branches ->
          do  temp <- Names.generate
              oexpr <- go expr
              case oexpr of
                Opt.VarLocal root ->
                  Case.optimize temp root <$> traverse (goBranch root) (branch:branches)

                _ ->
                  do  obranches <- traverse (goBranch temp) (branch:branches)
                      return $ Opt.Let (Opt.Def temp oexpr) (Case.optimize temp temp obranches)

        Can.Access record (A.At _ f) ->
          do  optRecord <- go record
              Names.registerField f (Opt.Access optRecord f)

        Can.Accessor f    -> Names.registerField f (Opt.Accessor f)
        Can.Update _ e fs -> Names.registerFields fs Opt.Update <*> go e <*> NEM.traverse goUpdate (F.toNEM fs)
        Can.Record fs     -> Names.registerFields fs Opt.Record <*> NEM.traverse go (F.toNEM fs)
        Can.Empty         -> pure Opt.Empty
        Can.Unit          -> Names.registerKernel Module.kernel_utils Opt.Unit
        Can.Pair   a b    -> Names.registerKernel Module.kernel_utils Opt.Pair   <*> go a <*> go b
        Can.Triple a b c  -> Names.registerKernel Module.kernel_utils Opt.Triple <*> go a <*> go b <*> go c

        Can.Shader src (Shader.Types attributes uniforms _varyings) ->
          pure (Opt.Shader src (Map.keysSet attributes) (Map.keysSet uniforms))

    goThen (c,b) =
      (,) <$> go c <*> go b

    goBranch root (Can.Branch p b) =
      do  destructors <- destructCase root p
          obranch <- go b
          pure (p, foldr Opt.Destruct obranch destructors)

    goUpdate (Can.Update_ _ e) =
      go e



-- DEFINITION


optLetValue :: Cycle -> Can.Value -> Opt.Expr -> Names.Tracker Opt.Expr
optLetValue cycle value body =
  case value of
    Can.Value_ n e     -> go n e
    Can.Value  n e _ _ -> go n e
  where
    go (A.At _ name) expr =
      do  oexpr <- optimize cycle expr
          pure $ Opt.Let (Opt.Def name oexpr) body


optLetFunc :: Cycle -> Can.Func -> Opt.Expr -> Names.Tracker Opt.Expr
optLetFunc cycle func body =
  case func of
    Can.Func_ n x xs e     -> go n x xs e
    Can.Func  n x xs e _ _ -> go n (Can._arg x) (map Can._arg xs) e
  where
    go (A.At _ name) x xs expr =
      do  (y, ys, destructors) <- destructArgs x xs
          oexpr                <- optimize cycle expr
          let ofunc = Opt.Function y ys (foldr Opt.Destruct oexpr destructors)
          pure $ Opt.Let (Opt.Def name ofunc) body



-- DESTRUCTURING


destructArgs :: Can.Pattern -> [Can.Pattern] -> Names.Tracker (N.Name, [N.Name], [Opt.Destructor])
destructArgs x xs =
  do  (y , ds ) <- destruct x
      (ys, dss) <- unzip <$> traverse destruct xs
      return (y, ys, concat (ds:dss))


destructCase :: N.Name -> Can.Pattern -> Names.Tracker [Opt.Destructor]
destructCase rootName pattern =
  reverse <$> destructHelp (Opt.Root rootName) pattern []


destruct :: Can.Pattern -> Names.Tracker (N.Name, [Opt.Destructor])
destruct pattern@(A.At _ ptrn) =
  case ptrn of
    Can.PVar name ->
      pure (name, [])

    Can.PAlias subPattern name ->
      do  revDs <- destructHelp (Opt.Root name) subPattern []
          pure (name, reverse revDs)

    _ ->
      do  name <- Names.generate
          revDs <- destructHelp (Opt.Root name) pattern []
          pure (name, reverse revDs)


destructHelp :: Opt.Path -> Can.Pattern -> [Opt.Destructor] -> Names.Tracker [Opt.Destructor]
destructHelp path (A.At region pattern) revDs =
  case pattern of
    Can.PAnything           -> pure revDs
    Can.PVar name           -> pure (Opt.Destructor name path : revDs)
    Can.PRecord fs          -> Names.registerFields_ fs (NEM.foldrWithKey consField revDs fs)
    Can.PAlias p n          -> destructHelp (Opt.Root n) p (Opt.Destructor n path : revDs)
    Can.PUnit               -> pure revDs
    Can.PPair   a b         -> destructTwo path a b revDs
    Can.PTriple a b c       -> destructThree path a b c revDs
    Can.PList []            -> pure revDs
    Can.PList (hd:tl)       -> destructTwo path hd (A.At region (Can.PList tl)) revDs
    Can.PCons  hd tl        -> destructTwo path hd tl revDs
    Can.PChr  _             -> pure revDs
    Can.PStr  _             -> pure revDs
    Can.PInt  _             -> pure revDs
    Can.PBool _ _ _ _       -> pure revDs
    Can.PVariant _ _ _ ps   -> destructVariant path ps revDs
    Can.PEnum    _ _ _      -> pure revDs
    Can.PVector  _ _ a b cs -> destructVariant path (a:b:cs) revDs
    Can.PBox     _ _ _ p    -> destructHelp (Opt.Unbox path) p revDs
    Can.PVoid    _ _        -> pure revDs
  where
    consField name () revs =
      Opt.Destructor name (Opt.Field name path) : revs


destructTwo :: Opt.Path -> Can.Pattern -> Can.Pattern -> [Opt.Destructor] -> Names.Tracker [Opt.Destructor]
destructTwo path a b revDs =
  case path of
    Opt.Root _ ->
      destructHelp (Opt.Index Index.second path) b =<<
      destructHelp (Opt.Index Index.first  path) a revDs

    _ ->
      (>>=) Names.generate $ \name ->
        let newRoot = Opt.Root name in
        destructHelp (Opt.Index Index.second newRoot) b =<<
        destructHelp (Opt.Index Index.first  newRoot) a (Opt.Destructor name path : revDs)


destructThree :: Opt.Path -> Can.Pattern -> Can.Pattern -> Can.Pattern -> [Opt.Destructor] -> Names.Tracker [Opt.Destructor]
destructThree path a b c revDs =
  case path of
    Opt.Root _ ->
      destructHelp (Opt.Index Index.third  path) c =<<
      destructHelp (Opt.Index Index.second path) b =<<
      destructHelp (Opt.Index Index.first  path) a revDs

    _ ->
      (>>=) Names.generate $ \name ->
        let newRoot = Opt.Root name in
        destructHelp (Opt.Index Index.third  newRoot) c =<<
        destructHelp (Opt.Index Index.second newRoot) b =<<
        destructHelp (Opt.Index Index.first  newRoot) a (Opt.Destructor name path : revDs)


destructVariant :: Opt.Path -> [Can.PArg] -> [Opt.Destructor] -> Names.Tracker [Opt.Destructor]
destructVariant path args revDs =
  case path of
    Opt.Root _ ->
      foldM (destructPArg path) revDs args

    _ ->
      do  name <- Names.generate
          foldM (destructPArg (Opt.Root name)) (Opt.Destructor name path : revDs) args


destructPArg :: Opt.Path -> [Opt.Destructor] -> Can.PArg -> Names.Tracker [Opt.Destructor]
destructPArg path revDs (Can.PArg index _ arg) =
  destructHelp (Opt.Index index path) arg revDs



-- TAIL CALL


optSelfRecursive :: Cycle -> Can.Func -> Names.Tracker Opt.Def
optSelfRecursive cycle func =
  case func of
    Can.Func_ (A.At _ n) x xs e     -> optSelfRec cycle n x xs e
    Can.Func  (A.At _ n) x xs e _ _ -> optSelfRec cycle n (Can._arg x) (map Can._arg xs) e


optSelfRec :: Cycle -> N.Name -> Can.Pattern -> [Can.Pattern] -> Can.Expr -> Names.Tracker Opt.Def
optSelfRec cycle name x xs expr =
  do  (y, ys, destructors) <- destructArgs x xs
      go y ys destructors <$> optTailCall cycle name y ys expr
  where
    go y ys destructors body =
      if hasTailCall body
      then Opt.TailDef name y ys (foldr Opt.Destruct body destructors)
      else Opt.Def name (Opt.Function y ys (foldr Opt.Destruct body destructors))


optTailCall :: Cycle -> N.Name -> N.Name -> [N.Name] -> Can.Expr -> Names.Tracker Opt.Expr
optTailCall cycle rootName arg args =
    go
  where
    go locExpr@(A.At _ expression) =
      case expression of
        Can.Call f x xs ->
          if isSelfCall f && length xs == length args
          then
            do  oarg  <- optimize cycle x
                oargs <- traverse (optimize cycle) xs
                pure $ Opt.TailCall rootName (Opt.TailAssign arg oarg) (zipWith Opt.TailAssign args oargs)
          else
            Opt.Call <$> optimize cycle f <*> optimize cycle x <*> traverse (optimize cycle) xs

        Can.If bs f       -> Opt.If <$> traverse goThen bs <*> go f
        Can.LetValue v b  -> optLetValue cycle v =<< go b
        Can.LetFunc  f b  -> optLetFunc  cycle f =<< go b
        Can.LetRec f [] b -> Opt.Let <$> optSelfRecursive cycle f <*> go b
        Can.LetRec f fs b ->
          do  obody <- go b
              foldM (\bod func -> optLetFunc cycle func bod) obody (f:fs)

        Can.LetDestruct pattern expr body ->
          do  (dname, destructors) <- destruct pattern
              oexpr <- optimize cycle expr
              obody <- go body
              pure $ Opt.Let (Opt.Def dname oexpr) (foldr Opt.Destruct obody destructors)

        Can.Case expr branch branches ->
          do  temp <- Names.generate
              oexpr <- optimize cycle expr
              case oexpr of
                Opt.VarLocal root ->
                  Case.optimize temp root <$> traverse (goBranch root) (branch:branches)

                _ ->
                  do  obranches <- traverse (goBranch temp) (branch:branches)
                      return $ Opt.Let (Opt.Def temp oexpr) (Case.optimize temp temp obranches)

        _ ->
          optimize cycle locExpr

    goThen (c,b) =
      (,) <$> optimize cycle c <*> go b

    goBranch root (Can.Branch p b) =
      do  destructors <- destructCase root p
          obranch <- go b
          pure (p, foldr Opt.Destruct obranch destructors)

    isSelfCall (A.At _ func) =
      case func of
        Can.VarLocal      n -> rootName == n
        Can.VarTopLevel _ n -> rootName == n
        _                   -> False



-- DETECT TAIL CALLS


hasTailCall :: Opt.Expr -> Bool
hasTailCall expression =
  case expression of
    Opt.TailCall _ _ _ -> True
    Opt.If bs f        -> hasTailCall f || any (hasTailCall . snd) bs
    Opt.Let _ b        -> hasTailCall b
    Opt.Destruct _ b   -> hasTailCall b
    Opt.Case _ _ d js  -> decidecHasTailCall d || any (hasTailCall . snd) js
    _                  -> False


decidecHasTailCall :: Opt.Decider Opt.Choice -> Bool
decidecHasTailCall decider =
  case decider of
    Opt.Chain _ t f   -> decidecHasTailCall t || decidecHasTailCall f
    Opt.FanOut _ ts f -> decidecHasTailCall f || any (decidecHasTailCall . snd) ts
    Opt.Leaf c        ->
      case c of
        Opt.Inline e -> hasTailCall e
        Opt.Jump _   -> False
