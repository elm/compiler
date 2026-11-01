{-# LANGUAGE BangPatterns, MagicHash #-}
module Type.Constrain.Pattern
  ( State(..)
  , emptyState
  , add
  )
  where


import Control.Monad (foldM)
import qualified Data.Index as Index
import qualified Data.Map.Strict as Map
import qualified Data.NonEmptyMap as NEM

import qualified Array as A

import qualified AST.Canonical as Can
import qualified AST.Prim.Fields as F
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeMask as M
import qualified AST.Prim.TypeName as T (list)
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as E
import Type.Type as T



-- ACTUALLY ADD CONSTRAINTS


-- The constraints are stored in reverse order so that adding a new
-- constraint is O(1) and we can reverse it at some later time.
--
data State =
  State
    { _headers :: Header
    , _vars :: [Variable]
    , _revCons :: [Constraint]
    }


type Header = Map.Map N.Name (A.Located Type)


{-# NOINLINE emptyState #-}
emptyState :: State
emptyState =
  State Map.empty [] []



-- ADD PATTERN


add :: Can.Pattern -> E.PExpected Type -> State -> IO State
add (A.At r pattern) e s =
  case pattern of
    Can.PAnything            -> return s
    Can.PVar x               -> return  $ addVar r x e s
    Can.PAlias p x           -> add p e $ addVar r x e s
    Can.PInt _               -> addInt r e s
    Can.PBool _ _ _ _        -> addLit r E.PBool T.bool   e s
    Can.PChr _               -> addLit r E.PChr  T.char   e s
    Can.PStr _               -> addLit r E.PStr  T.string e s
    Can.PUnit                -> addLit r E.PUnit UnitN    e s
    Can.PPair   a b          -> addPair   r a b   e s
    Can.PTriple a b c        -> addTriple r a b c e s
    Can.PRecord fs           -> addRecord r fs e s
    Can.PList   ps           -> addList   r   ps e s
    Can.PCons p ps           -> addCons   r p ps e s
    Can.PVariant n pt _ xs   -> addVariant r n pt xs e s
    Can.PEnum    n pt _      -> addVariant r n pt [] e s
    Can.PVector  n pt a b cs -> addVariant r n pt (a:b:cs) e s
    Can.PBox     n pt t p    -> addVariant r n pt [box t p] e s
    Can.PVoid    n pt        -> addVariant r n pt [] e s
  where
    box = Can.PArg Index.first


addLit :: A.Region -> E.PCategory -> Type -> E.PExpected Type -> State -> IO State
addLit region category tipe expected (State headers vars revCons) =
  do  let con = CPattern region category tipe expected
      return $ State headers vars (con:revCons)


addInt :: A.Region -> E.PExpected Type -> State -> IO State
addInt region expected (State headers vars revCons) =
  do  var <- mkFlexNumber
      let con = CPattern region E.PInt (VarN var) expected
      return $ State headers (var:vars) (con:revCons)



-- ADD VARIABLE


addVar :: A.Region -> N.Name -> E.PExpected Type -> State -> State
addVar region name expected (State headers vars revCons) =
  let
    tipe = getType expected
    newHeaders = Map.insert name (A.At region tipe) headers
  in
  State newHeaders vars revCons


getType :: E.PExpected Type -> Type
getType expected =
  case expected of
    E.PNoExpectation   tipe -> tipe
    E.PFromContext _ _ tipe -> tipe



-- ADD TUPLES


addPair :: A.Region -> Can.Pattern -> Can.Pattern -> E.PExpected Type -> State -> IO State
addPair region a b expected state =
  do  aVar <- mkFlexAny
      bVar <- mkFlexAny
      let aType = VarN aVar
      let bType = VarN bVar
      (State headers vars revCons) <- simpleAdd b bType =<< simpleAdd a aType state
      let tupleCon = CPattern region E.PTuple (PairN aType bType) expected
      return $ State headers (aVar:bVar:vars) (tupleCon:revCons)


addTriple :: A.Region -> Can.Pattern -> Can.Pattern -> Can.Pattern -> E.PExpected Type -> State -> IO State
addTriple region a b c expected state =
  do  aVar <- mkFlexAny
      bVar <- mkFlexAny
      cVar <- mkFlexAny
      let aType = VarN aVar
      let bType = VarN bVar
      let cType = VarN cVar
      (State headers vars revCons) <- simpleAdd c cType =<< simpleAdd b bType =<< simpleAdd a aType state
      let tupleCon = CPattern region E.PTuple (TripleN aType bType cType) expected
      return $ State headers (aVar:bVar:cVar:vars) (tupleCon:revCons)


simpleAdd :: Can.Pattern -> Type -> State -> IO State
simpleAdd pattern patternType state =
  add pattern (E.PNoExpectation patternType) state



-- ADD RECORD


addRecord :: A.Region -> NEM.Map N.Name () -> E.PExpected Type -> State -> IO State
addRecord region fields expected (State headers vars revCons) =
  do  extVar    <- mkFlexAny
      fieldVars <- NEM.traverse (\() -> mkFlexAny) fields
      let fieldTypes = NEM.map VarN fieldVars
      let recordType = ExtRecN order fieldTypes (VarN extVar)
      let recordCon  = CPattern region E.PRecord recordType expected
      return $
        State
          { _headers = Map.union headers (Map.map (A.At region) (NEM.toDict fieldTypes))
          , _vars    = NEM.foldr (:) (extVar:vars) fieldVars
          , _revCons = recordCon : revCons
          }
  where
    !(F.Order order) = F.unordered



-- ADD LIST


addList :: A.Region -> [Can.Pattern] -> E.PExpected Type -> State -> IO State
addList region patterns expected state =
  do  entryVar <- mkFlexAny
      let entryType = VarN entryVar
      let listType = AppN ModuleName.list T.list M.list (A.from1 entryType)

      (State headers vars revCons) <-
        foldM (addEntry entryType) state (Index.indexedMap (,) patterns)

      let listCon = CPattern region E.PList listType expected
      return $ State headers (entryVar:vars) (listCon:revCons)
  where
    addEntry t s (i, p) =
      add p (E.PFromContext region (E.PListEntry i) t) s


addCons :: A.Region -> Can.Pattern -> Can.Pattern -> E.PExpected Type -> State -> IO State
addCons region headPattern tailPattern expected state =
  do  entryVar <- mkFlexAny
      let entryType = VarN entryVar
      let listType = AppN ModuleName.list T.list M.list (A.from1 entryType)

      let headExpectation = E.PNoExpectation entryType
      let tailExpectation = E.PFromContext region E.PTail listType

      (State headers vars revCons) <-
        add headPattern headExpectation =<<
          add tailPattern tailExpectation state

      let listCon = CPattern region E.PList listType expected
      return $ State headers (entryVar:vars) (listCon : revCons)



-- ADD VARIANT


addVariant :: A.Region -> N.Name -> Can.PType -> [Can.PArg] -> E.PExpected Type -> State -> IO State
addVariant region name (Can.PType h n m typeVars _) args expected state =
  do  ctorVars <- A.traverseIO varToFlex typeVars

      (State headers vars revCons) <- foldM (addPArg region name ctorVars) state args

      let ctorType = AppN h n m (A.map VarN ctorVars)
      let ctorCon  = CPattern region (E.PCtor name) ctorType expected

      return $ State headers (A.addToList ctorVars vars) (ctorCon:revCons)


addPArg :: A.Region -> N.Name -> A.Array Variable -> State -> Can.PArg -> IO State
addPArg region name ctorVars state (Can.PArg index tipe pattern) =
  let
    expected =
      E.PFromContext region (E.PCtorArg name index) (T.TypeN ctorVars tipe)
  in
  add pattern expected state

