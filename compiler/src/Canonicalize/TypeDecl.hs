{-# LANGUAGE Rank2Types, TemplateHaskell #-}
module Canonicalize.TypeDecl
  ( Result(..)
  , Exposed(..)
  , addTypes
  )
  where


import Control.Monad (foldM)
import qualified Data.Dups as Dups
import qualified Data.List as List
import qualified Data.List.Utils as List
import qualified Data.Map.Strict as Map
import qualified Data.Map.Utils as Map
import qualified Data.OneOrMore as OOM

import qualified Array as A
import qualified Graph

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified AST.Prim.Fields as F
import qualified AST.Prim.Name as N
import qualified AST.Prim.Tag as Tag
import qualified AST.Prim.TypeIndex as T
import qualified AST.Prim.TypeMask as M
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Canonicalize.Environment as Env
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Nitpick.Utils as Nit
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as E



-- ADD TYPES
--
-- TODO disallow type constraints in phantom type variables of `type`?


newtype Result a =
  Result (forall r. (a -> r) -> (OOM.OneOrMore E.Problem -> r) -> r)


newtype Exposed =
  Exposed (Dups.Dict T.Name A.Region Src.Privacy)


addTypes :: [A.Located Src.Union] -> [A.Located Src.Alias] -> Env.Env -> Result (Env.Env, Map.Map T.Name Can.TypeDecl)
addTypes unions aliases env@(Env.Env home vs ts cs bs qvs qts qcs) =
  do  localDict  <- checkTypeDecls unions aliases
      localGraph <- partiallyCanonicalize env localDict

      (newTypes, newCtors, types) <- finalizeTypes home localGraph

      let ts'  = Map.union newTypes ts
      let cs'  = Map.union newCtors cs
      let env' = Env.Env home vs ts' cs' bs qvs qts qcs

      return (env', types)



--------------------------------------------------------------------------------
-- PHASE 1 ---------------------------------------------------------------------
--------------------------------------------------------------------------------


-- RUN BASIC CHECKS
--
--   1. check for duplicate type names
--   2. assign symbols to unions/aliases
--   3. check for duplicate type variable names
--   4. check for unused type variables


data Local1
  = Union1 (A.Located Src.Union) Env.Arity
  | Alias1 (A.Located Src.Alias) Env.Arity


checkTypeDecls :: [A.Located Src.Union] -> [A.Located Src.Alias] -> Result (Map.Map T.Name Local1)
checkTypeDecls unions aliases =
  do  let udups = List.foldl' insertUnion Dups.none unions
      let adups = List.foldl' insertAlias Dups.none aliases
      (us, as) <- dedups E.DuplicateType udups adups
      unions1  <- traverse checkUnionVars us
      aliases1 <- traverse checkAliasVars as
      return $ Map.union unions1 aliases1
  where
    insertUnion dups u@(A.At _ (Src.Union (A.At r n) _ _ _)) = Dups.insert n r u dups
    insertAlias dups a@(A.At _ (Src.Alias (A.At r n) _ _  )) = Dups.insert n r a dups



-- CHECK UNION VARS


checkUnionVars :: A.Located Src.Union -> Result Local1
checkUnionVars union@(A.At unionRegion (Src.Union (A.At _ name) vars variant variants)) =
  let
    addUnionVar  vs (A.At region arg)  = Dups.insert arg region region vs
    addAssocVars vs (Src.Variant _ ts) = List.foldl' addVars vs ts
  in
  do  boundVars <- dedup (E.DuplicateUnionArg name) (List.foldl' addUnionVar Dups.none vars)
      let freeVars = List.foldl' addAssocVars Map.empty (variant:variants)
      case Map.toListWith (flip A.At) (Map.difference freeVars boundVars) of
        [] ->
          return $ Union1 union (Env.Arity (Map.size boundVars))

        unbound:unbounds ->
          throw $
            E.TypeVarsUnboundInUnion unionRegion name (List.map A.toValue vars) unbound unbounds



-- CHECK ALIAS VARS


checkAliasVars :: A.Located Src.Alias -> Result Local1
checkAliasVars alias@(A.At aliasRegion (Src.Alias (A.At _ name) args tipe)) =
  let
    addArg dict (A.At region arg) = Dups.insert arg region region dict
  in
  do  boundVars <- dedup (E.DuplicateAliasArg name) (List.foldl' addArg Dups.none args)
      let freeVars = addVars Map.empty tipe
      let overlap = Map.size (Map.intersection boundVars freeVars)
      if Map.size boundVars == overlap && Map.size freeVars == overlap
        then return $ Alias1 alias (Env.Arity (Map.size boundVars))
        else
          throw $
            E.TypeVarsMessedUpInAlias aliasRegion name
              (List.map A.toValue args)
              (Map.toListWith (flip A.At) (Map.difference boundVars freeVars))
              (Map.toListWith (flip A.At) (Map.difference freeVars boundVars))



-- ADD VARS


addVars :: Map.Map T.Var A.Region -> Src.Type -> Map.Map T.Var A.Region
addVars vars (A.At region tipe) =
  case tipe of
    Src.TLambda x y zs     -> List.foldl' addVars (addVars (addVars vars x) y) zs
    Src.TVar x             -> Map.insert x region vars
    Src.TType _ _ xs       -> List.foldl' addVars vars xs
    Src.TTypeQual _ _ _ xs -> List.foldl' addVars vars xs
    Src.TUnit              -> vars
    Src.TTuple a b cs      -> List.foldl' addVars (addVars (addVars vars a) b) cs
    Src.TEmpty             -> vars
    Src.TRecord f fs       -> List.foldl' addField (addField vars f) fs
    Src.TExtRec f fs ext   -> List.foldl' addField (addField (addExt vars ext) f) fs
  where
    addField vs (_,t)    = addVars vs t
    addExt vs (A.At r e) = Map.insert e r vs



--------------------------------------------------------------------------------
-- PHASE 2 ---------------------------------------------------------------------
--------------------------------------------------------------------------------


-- PARTIALLY CANONICALIZED TYPES
--
-- This pass fully canonicalizes foreign types and partially canonicalizes
-- local types.
--
-- Figuring out if local types are equatable is tricky. Imagine mutually
-- recursive types A and B. If they only contain equatable types, then both
-- A and B are equatable. But if either contains a single not-equatable
-- type, then neither A nor B are equatable.
--
-- This is resolved by having an intermediate type representation with an extra
-- TLocal variant that can be upgraded to TType or TAlias in a second pass.


data Local2
  = Union2 Union
  | Alias2 Alias


data Union = Union (A.Located T.Name) (A.Array T.Var) Variants
data Alias = Alias (A.Located T.Name) (A.Array T.Var) Src.Type Type


data Type
  = TLambda Type Type [Type]
  | TVar T.Index
  | TType  ModuleName.Canonical T.Name M.Mask (A.Array Type)
  | TAlias ModuleName.Canonical T.Name M.Mask (A.Array Type) I.Aliased
  | TLocal T.Name (A.Array Type)
  | TUnit
  | TPair   Type Type
  | TTriple Type Type Type
  | TEmpty
  | TRecord (F.Fields Type)
  | TExtRec (F.Fields Type) T.Index


data Variants
  = Variants (Tag.Table Variant)
  | Enum     (Tag.Table (A.Located N.Name))
  | Vector (A.Located N.Name) Type Type [Type]
  | Box    (A.Located N.Name) Type
  | Void   (A.Located N.Name)


data Variant =
  Variant (A.Located N.Name) [Type]



-- PARTIALLY CANONICALIZE


data TEnv =
  TEnv
    { _t_env :: Env.Env
    , _t_vars :: T.Registry T.Var
    , _t_locals :: Map.Map T.Name Local1
    }


partiallyCanonicalize :: Env.Env -> Map.Map T.Name Local1 -> Result [Graph.Node T.Name Local2]
partiallyCanonicalize env locals =
    traverse go (Map.elems locals)
  where
    go local1 =
      case local1 of
        Union1 (A.At _ (Src.Union n xs v vs)) _ -> run n xs $ \vars tenv -> Union2 . Union n vars <$> partVariants tenv v vs
        Alias1 (A.At _ (Src.Alias n xs tipe)) _ -> run n xs $ \vars tenv -> Alias2 . Alias n vars tipe <$> partType tenv tipe

    run (A.At _ name) xs mkPhase2 =
      Result $ \ok err ->
        let
          vars = A.fromListWith A.toValue xs
          tenv = TEnv env (T.fromArray vars) locals
        in
        case mkPhase2 vars tenv of
          Phase2 k ->
            k [] (\edges local2 -> ok (Graph.Node name local2 edges)) err



-- PART VARIANTS


partVariants :: TEnv -> Src.Variant -> [Src.Variant] -> Phase2 Variants
partVariants tenv variant variants =
  case (variant, variants) of
    (Src.Variant n xs, []) ->
      case xs of
        []     -> pure $ Void n
        [t]    -> Box    n <$> go t
        a:b:cs -> Vector n <$> go a <*> go b <*> traverse go cs

    (a,b:cs) ->
      case (,,) <$> goE a <*> goE b <*> traverse goE cs of
        Just (x,y,zs) ->
          pure $ Enum $ Tag.table x y zs

        Nothing ->
          Variants <$> (Tag.table <$> goV a <*> goV b <*> traverse goV cs)
  where
    go                     = partType tenv
    goV (Src.Variant n xs) = Variant n <$> traverse go xs
    goE (Src.Variant n xs) =
      case xs of
        []  -> Just n
        _:_ -> Nothing



-- PART TYPES


partType :: TEnv -> Src.Type -> Phase2 Type
partType tenv@(TEnv env vars _) =
    go
  where
    go (A.At region tipe) =
      case tipe of
        Src.TUnit              -> pure TUnit
        Src.TEmpty             -> pure TEmpty
        Src.TVar x             -> pure $ TVar (T.lookup x vars)
        Src.TTuple a b cs      -> goTuple region a b cs
        Src.TRecord f fs       -> TRecord    <$> partFields tenv f fs
        Src.TExtRec f fs e     -> goExtRec e <$> partFields tenv f fs
        Src.TType     r   n xs -> partLocal  tenv region r n xs
        Src.TTypeQual r h n xs -> Env.withTypeQual env r h n (goUnion region n xs) (goAlias region n xs) throw2
        Src.TLambda x y ys     -> TLambda <$> go x <*> go y <*> traverse go ys

    goUnion r n xs h (Env.UnionInfo m a  ) =         TType  h n m       <$> partArgs tenv r n a xs
    goAlias r n xs h (Env.AliasInfo m a t) = (\ys -> TAlias h n m ys t) <$> partArgs tenv r n a xs

    goTuple r a b cs =
      case cs of
        []    -> TPair   <$> go a <*> go b
        [c]   -> TTriple <$> go a <*> go b <*> go c
        _:_:_ -> throw2 $ E.TupleLargerThanThree r

    goExtRec (A.At _ ext) fields =
      TExtRec fields (T.lookup ext vars)


partLocal :: TEnv -> A.Region -> A.Region -> T.Name -> [Src.Type] -> Phase2 Type
partLocal tenv@(TEnv env _ locals) region nameRegion name args =
  case Map.lookup name locals of
    Just local1 ->
      Phase2 $ \edges ok err ->
        case partArgs tenv region name (getArity local1) args of
          Phase2 k ->
            k edges (\es xs -> ok (name:es) (TLocal name xs)) err

    Nothing ->
      Env.withType env nameRegion name goUnion goAlias throw2
  where
    getArity local1 =
      case local1 of
        Union1 _ arity -> arity
        Alias1 _ arity -> arity

    goUnion h (Env.UnionInfo m a  ) =         TType  h name m       <$> partArgs tenv region name a args
    goAlias h (Env.AliasInfo m a t) = (\xs -> TAlias h name m xs t) <$> partArgs tenv region name a args


partArgs :: TEnv -> A.Region -> T.Name -> Env.Arity -> [Src.Type] -> Phase2 (A.Array Type)
partArgs tenv region name (Env.Arity expectedArity) args =
  let
    actualArity = List.length args
  in
  if expectedArity == actualArity
  then A.fromList <$> traverse (partType tenv) args
  else throw2 $ E.BadArity_Type region name expectedArity actualArity


type SrcField =
  (A.Located N.Name, Src.Type)


partFields :: TEnv -> SrcField -> [SrcField] -> Phase2 (F.Fields Type)
partFields tenv f fs =
  case Dups.detect1 (\_ _ t -> t) E.DuplicateField $ List.foldl' add (one f) fs of
    Dups.Detector k ->
      k (F.fromOrdered (partType tenv)) throws2
  where
    one   (A.At r n, t) = Dups.one1    n r (F.Ordered 0 t)
    add d (A.At r n, t) = Dups.insert1 n r (F.Ordered (fromIntegral (Dups.size1 d)) t) d



--------------------------------------------------------------------------------
-- PHASE 3 ---------------------------------------------------------------------
--------------------------------------------------------------------------------


-- FINALIZE TYPES
--
--   1. check for recursive aliases
--   2. finalize serialization status
--   3. prepare for additions to Env


type Types = Map.Map T.Name (Env.Type Env.UnionInfo Env.AliasInfo)
type Ctors = Map.Map N.Name (Env.Type Env.UnionCtor Env.AliasCtor)


finalizeTypes :: ModuleName.Canonical -> [Graph.Node T.Name Local2] -> Result (Types, Ctors, Map.Map T.Name Can.TypeDecl)
finalizeTypes home nodes =
  do  (State dups decls locals) <- foldM (addSCC home) empty (Graph.toSCC nodes)
      ctors <- dedup E.DuplicateCtor dups
      return (Map.map toTypeInfo locals, ctors, decls)
  where
    toTypeInfo local3 =
      case local3 of
        Union3 u -> Env.Union home u
        Alias3 a -> Env.Alias home a



-- ADD NODE


data State =
  State
    { _ctors  :: Dups.Dict N.Name A.Region (Env.Type Env.UnionCtor Env.AliasCtor)
    , _decls  :: Map.Map T.Name Can.TypeDecl
    , _locals :: Map.Map T.Name Local3
    }


data Local3
  = Union3 Env.UnionInfo
  | Alias3 Env.AliasInfo


empty :: State
empty =
  State Dups.none Map.empty Map.empty


addSCC :: ModuleName.Canonical -> State -> Graph.SCC T.Name Local2 -> Result State
addSCC home state@(State ctors decls locals) scc =
  case scc of
    Graph.Acyclic (Graph.Node _ local2 _) ->
      case local2 of
        Alias2 alias@(Alias _ _ _ tipe) ->
          return $ addAlias home (maskType locals tipe) alias state

        Union2 union@(Union (A.At _ name) vars variants) ->
          let
            mask      = maskVariants home name locals variants
            unionInfo = Env.UnionInfo mask (Env.Arity (A.size vars))
            newLocals = Map.insert name (Union3 unionInfo) locals
            info      = I.Union mask vars (finalizeVariants home newLocals variants)
            newDecls  = Map.insert name (Can.Union info) decls
            newCtors  = Dups.union ctors $ toCtors home newLocals mask union
          in
          return $ State newCtors newDecls newLocals

    Graph.Cyclic component ->
      do  (aliases, unions) <- checkRecursion component
          let mask    = M.cycle (maskNode home locals) component
          let locals1 = Map.union locals $ Map.fromList $ List.map (goLocal mask) unions
          let (State cs ds newLocals) = List.foldl' (goAlias mask) (State ctors decls locals1) aliases
          let newCtors = Dups.unions (cs : List.map (toCtors home newLocals mask) unions)
          let newDecls = Map.union ds $ Map.fromList $ List.map (goUnion mask newLocals) unions
          return $ State newCtors newDecls newLocals
      where
        goLocal mask (Union (A.At _ name) vars _) =
          (name, Union3 (Env.UnionInfo mask (Env.Arity (A.size vars))))

        goAlias mask state' alias =
          addAlias home mask alias state'

        goUnion mask newLocals (Union (A.At _ name) vars variants) =
          (name, Can.Union (I.Union mask vars (finalizeVariants home newLocals variants)))



-- ADD ALIAS


addAlias :: ModuleName.Canonical -> M.Mask -> Alias -> State -> State
addAlias home mask (Alias (A.At region name) vars _ localType) (State ctors decls locals) =
  case localType of
    TLambda x y ys    -> goNormal $ I.TLambda (go x) (go y) (List.map go ys)
    TVar i            -> goNormal $ I.TVar i
    TType  h n m xs   -> goNormal $ I.TType  h n m (A.map go xs)
    TAlias h n m xs t -> goNormal $ I.TAlias h n m (A.map go xs) t
    TLocal n xs       -> goNormal $ goLocal n xs
    TUnit             -> goNormal $ I.TUnit
    TPair   a b       -> goNormal $ I.TPair   (go a) (go b)
    TTriple a b c     -> goNormal $ I.TTriple (go a) (go b) (go c)
    TEmpty            -> goNormal $ I.TEmpty
    TRecord fs        -> goRecord fs
    TExtRec fs e      -> goNormal $ I.TExtRec (F.map go fs) e
  where
    go = finalizeType home locals

    goLocal n xs =
      case $(Map.require 'addAlias) n locals T.nameToChars of
        Union3 (Env.UnionInfo m _  ) -> I.TType  home n m (A.map go xs)
        Alias3 (Env.AliasInfo m _ t) -> I.TAlias home n m (A.map go xs) t

    goNormal tipe =
      let
        aliased   = I.Aliased tipe
        info      = Env.AliasInfo mask (Env.Arity (A.size vars)) aliased
        newDecls  = Map.insert name (Can.Alias (I.Alias mask vars aliased)) decls
        newLocals = Map.insert name (Alias3 info) locals
      in
      State ctors newDecls newLocals

    goRecord fields =
      let
        dict      = F.map go fields
        aliased   = I.Aliased (I.TRecord dict)
        (f,fs)    = F.flatten dict
        info      = Env.AliasInfo mask (Env.Arity (A.size vars)) aliased
        anno      = I.Forall vars $ lambda f fs (I.TAlias home name mask (T.imap I.TVar vars) aliased)
        ctor      = Env.Record anno (fst f) (List.map fst fs)
        newCtors  = Dups.insert (T.nameToName name) region (Env.Alias home ctor) ctors
        newDecls  = Map.insert name (Can.Alias (I.Alias mask vars aliased)) decls
        newLocals = Map.insert name (Alias3 info) locals
      in
      State newCtors newDecls newLocals

    lambda (_,t) fs result =
      case List.map snd fs of
        []   -> I.TLambda t result []
        x:xs -> I.TLambda t x (xs ++ [result])



-- CHECK RECURSION


checkRecursion :: Graph.Component T.Name Local2 -> Result ([Alias], [Union])
checkRecursion component =
  Graph.withComponent component $ \n ns ->
    go (n:ns) [] []
  where
    go nodes aliasNodes revUnions =
      case nodes of
        [] ->
          do  aliases <- checkAliasRecursion aliasNodes
              return (aliases, revUnions)

        Graph.Node name local2 edges : rest ->
          case local2 of
            Union2 union -> go rest aliasNodes (union:revUnions)
            Alias2 alias -> go rest (Graph.Node name alias edges : aliasNodes) revUnions



-- CHECK ALIAS RECURSION


checkAliasRecursion :: [Graph.Node T.Name Alias] -> Result [Alias]
checkAliasRecursion aliasNodes =
    traverse go (Graph.toSCC aliasNodes)
  where
    go scc =
      case scc of
        Graph.Acyclic (Graph.Node _ pair _) ->
          return pair

        Graph.Cyclic component ->
          throw $ Graph.withMinimalCycle regionSelector component Graph._value badAliasRecursion

    regionSelector =
      Graph.RootSelector $ \node nodes ->
        let
          (Graph.Node k _ _, _) = List.sortBy1 toRegion node nodes
        in
        (k,())

    toRegion (Graph.Node _ (Alias (A.At region _) _ _ _) _) =
      region


badAliasRecursion :: () -> Graph.MinimalCycle Alias -> E.Problem
badAliasRecursion () (Graph.MinimalCycle alias@(Alias name args tipe _) aliases) =
    E.RecursiveAlias name (A.toList args) tipe (Graph.MinimalCycle (go alias) (List.map go aliases))
  where
    go (Alias (A.At _ n) _ _ _) = n



-- COMPUTE TYPE MASK


maskNode :: ModuleName.Canonical -> Map.Map T.Name Local3 -> Graph.Node T.Name Local2 -> M.Mask
maskNode home locals (Graph.Node _ local2 _) =
  case local2 of
    Union2 (Union n _ variants) -> maskVariants home (A.toValue n) locals variants
    Alias2 (Alias _ _ _ tipe)   -> maskType locals tipe


maskVariants :: ModuleName.Canonical -> T.Name -> Map.Map T.Name Local3 -> Variants -> M.Mask
maskVariants home name locals variants =
  case variants of
    Variants t      -> M.variants (maskType locals) go t
    Enum   _        -> M.enum
    Vector _ a b cs -> M.vector (maskType locals) a b cs
    Box    _ t      -> M.box (maskType locals) t
    Void   _        -> M.void home name
  where
    go (Variant _ args) = args


maskType :: Map.Map T.Name Local3 -> Type -> M.Mask
maskType locals tipe =
    go tipe
  where
    go t =
      case t of
        TLambda _ _ _     -> M.lambda
        TVar _            -> M.var
        TType  _ _ m xs   -> M.args m go xs
        TAlias _ _ m xs _ -> M.args m go xs
        TLocal n xs       -> M.args (goLocal n) go xs
        TUnit             -> M.unit
        TPair   a b       -> M.pairOf go a b
        TTriple a b c     -> M.tripleOf go a b c
        TEmpty            -> M.empty
        TRecord fs        -> M.recordOf go fs
        TExtRec fs _      -> M.recordOf go fs

    goLocal n =
      case Map.lookup n locals of
        Nothing -> M.var
        Just l3 ->
          case l3 of
            Union3 (Env.UnionInfo m _  ) -> m
            Alias3 (Env.AliasInfo m _ _) -> m



-- FINALIZE TYPES


finalizeVariants :: ModuleName.Canonical -> Map.Map T.Name Local3 -> Variants -> I.Variants
finalizeVariants home locals variants =
  case variants of
    Variants          vs     -> I.Variants $ Tag.map goV vs
    Enum              es     -> I.Enum     $ Tag.map A.toValue es
    Vector (A.At _ n) a b cs -> I.Vector n (go a) (go b) (List.map go cs)
    Box    (A.At _ n) t      -> I.Box    n (go t)
    Void   (A.At _ n)        -> I.Void   n
  where
    go = finalizeType home locals
    goV (Variant (A.At _ n) xs) = I.Variant n (List.map go xs)


finalizeType :: ModuleName.Canonical -> Map.Map T.Name Local3 -> Type -> I.Type
finalizeType home locals localType =
    go localType
  where
    go tipe =
      case tipe of
        TLambda x y zs    -> I.TLambda (go x) (go y) (List.map go zs)
        TVar i            -> I.TVar i
        TType  h n m xs   -> I.TType  h n m (A.map go xs)
        TAlias h n m xs t -> I.TAlias h n m (A.map go xs) t
        TLocal n xs       -> goLocal n xs
        TUnit             -> I.TUnit
        TPair   a b       -> I.TPair   (go a) (go b)
        TTriple a b c     -> I.TTriple (go a) (go b) (go c)
        TEmpty            -> I.TEmpty
        TRecord fs        -> I.TRecord (F.map go fs)
        TExtRec fs e      -> I.TExtRec (F.map go fs) e

    goLocal name xs =
      case $(Map.require 'finalizeType) name locals T.nameToChars of
        Union3 (Env.UnionInfo m _  ) -> I.TType  home name m (A.map go xs)
        Alias3 (Env.AliasInfo m _ t) -> I.TAlias home name m (A.map go xs) t



--------------------------------------------------------------------------------
-- CTORS -----------------------------------------------------------------------
--------------------------------------------------------------------------------


-- TO CTORS


toCtors :: ModuleName.Canonical -> Map.Map T.Name Local3 -> M.Mask -> Union -> Dups.Dict N.Name A.Region (Env.Type Env.UnionCtor a)
toCtors home locals mask (Union (A.At _ name) vars variants) =
  let
    go     = finalizeType home locals
    result = I.TType home name mask (T.imap I.TVar vars)
  in
  case variants of
    Variants table ->
      let
        ptype = Can.PType home name mask vars (vNits table)
        add tag (Variant (A.At r n) tipes) dups =
          let
            len  = List.length tipes
            args = List.map go tipes
            anno = mkAnno vars args result
          in
          Dups.insert n r (Env.Union home (Env.Variant anno ptype tag len args)) dups
      in
      Tag.foldlWithTag add Dups.none table

    Enum table ->
      let
        anno  = I.Forall vars result
        ptype = Can.PType home name mask vars (cNits table)
        add tag (A.At r n) dups =
          Dups.insert n r (Env.Union home (Env.Enum anno ptype tag)) dups
      in
      Tag.foldlWithTag add Dups.none table

    Vector (A.At r n) a b cs ->
      let
        x     = go a
        y     = go b
        zs    = List.map go cs
        len   = 2 + List.length cs
        anno  = I.Forall vars (I.TLambda x y (zs ++ [result]))
        ptype = Can.PType home name mask vars (Nit.vector len n)
      in
      Dups.one n r (Env.Union home (Env.Vector anno ptype len x y zs))

    Box (A.At r n) tipe ->
      let
        arg   = go tipe
        anno  = I.Forall vars (I.TLambda arg result [])
        ptype = Can.PType home name mask vars (Nit.box n)
      in
      Dups.one n r (Env.Union home (Env.Box anno ptype arg))

    Void (A.At r n) ->
      let
        anno  = I.Forall vars result
        ptype = Can.PType home name mask vars (Nit.void n)
      in
      Dups.one n r (Env.Union home (Env.Void anno ptype))


vNits :: Tag.Table Variant -> Nit.Variants
vNits table =
  Tag.unpack table Nit.Variants_ $ \t (Variant (A.At _ n) xs) ->
      Nit.Variant n t (List.length xs)


cNits :: Tag.Table (A.Located N.Name) -> Nit.Variants
cNits table =
  Tag.unpack table Nit.Variants_ $ \t (A.At _ n) ->
      Nit.Variant n t 0



-- MAKE ANNOTATION


mkAnno :: A.Array T.Var -> [I.Type] -> I.Type -> I.Annotation
mkAnno vars args result =
  I.Forall vars $
    case args of
      []     -> result
      [t]    -> I.TLambda t result []
      x:y:zs -> I.TLambda x y (zs ++ [result])



-- RESULT


throw :: E.Problem -> Result a
throw x =
  Result $ \_ err ->
    err (OOM.one x)


throws :: OOM.OneOrMore E.Problem -> Result a
throws xs =
  Result $ \_ err ->
    err xs


dedup :: (k -> r -> r -> E.Problem) -> Dups.Dict k r a -> Result (Map.Map k a)
dedup toError dups =
  case Dups.detect (\_ _ a -> a) toError dups of
    Dups.Detector k ->
      k return throws

dedups :: (Ord k) => (k -> r -> r -> E.Problem) -> Dups.Dict k r a -> Dups.Dict k r b -> Result (Map.Map k a, Map.Map k b)
dedups toError dups1 dups2 =
  case Dups.detects (\_ _ a -> a) (\_ _ b -> b) toError dups1 dups2 of
    Dups.Detector k ->
      k return throws


instance Functor Result where
  {-# INLINE fmap #-}
  fmap func (Result k) =
    Result $ \ok err ->
      k (ok . func) err


instance Applicative Result where
  {-# INLINE pure #-}
  pure a =
    Result $ \ok _ ->
      ok a

  {-# INLINE (<*>) #-}
  (<*>) (Result kF) (Result kA) =
    Result $ \ok err ->
      let
        okF func = kA (ok . func) err
        errF xs  = kA (\_ -> err xs) (err . OOM.more xs)
      in
      kF okF errF


instance Monad Result where
  {-# INLINE (>>=) #-}
  (>>=) (Result kA) callback =
    Result $ \ok err ->
      let
        okA a =
          case callback a of
            Result kB -> kB ok err
      in
      kA okA err



-- PHASE 2


newtype Phase2 a =
  Phase2 (forall r. [T.Name] -> ([T.Name] -> a -> r) -> (OOM.OneOrMore E.Problem -> r) -> r)


throw2 :: E.Problem -> Phase2 a
throw2 x =
  Phase2 $ \_ _ err ->
    err (OOM.one x)


throws2 :: OOM.OneOrMore E.Problem -> Phase2 a
throws2 xs =
  Phase2 $ \_ _ err ->
    err xs


instance Functor Phase2 where
  {-# INLINE fmap #-}
  fmap func (Phase2 k) =
    Phase2 $ \edges ok err ->
      let
        okF es arg = ok es (func arg)
      in
      k edges okF err


instance Applicative Phase2 where
  {-# INLINE pure #-}
  pure a =
    Phase2 $ \edges ok _ ->
      ok edges a

  {-# INLINE (<*>) #-}
  (<*>) (Phase2 kF) (Phase2 kA) =
    Phase2 $ \edges ok err ->
      let
        okF es func =
          let
            okA e arg = ok e (func arg)
          in
          kA es okA err

        errF xs =
          kA edges (\_ _ -> err xs) (err . OOM.more xs)
      in
      kF edges okF errF
