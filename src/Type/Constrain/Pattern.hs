{-# OPTIONS_GHC -Wall #-}
module Type.Constrain.Pattern
  ( State(..)
  , emptyState
  , add
  )
  where


import Control.Arrow (second)
import Control.Monad (foldM)
import qualified Data.Map.Strict as Map

import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified Data.Index as Index
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import Type.Constraint
import qualified Type.Instantiate as Instantiate
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


add :: Can.Pattern -> PatternExpectation -> State -> IO State
add (A.A region pattern) expectation state =
  case pattern of
    Can.PAnything ->
      return state

    Can.PVar name ->
      return $ addToHeaders region name expectation state

    Can.PAlias realPattern name ->
      add realPattern expectation $
        addToHeaders region name expectation state

    Can.PUnit ->
      do  let (State headers vars revCons) = state
          let unitCon = CPattern region PUnit UnitN expectation
          return $ State headers vars (unitCon:revCons)

    Can.PTuple a b maybeC ->
      addTuple region a b maybeC expectation state

    Can.PCtor home typeName typeVars ctorName args ->
      addCtor region home typeName typeVars ctorName args expectation state

    Can.PList patterns ->
      do  entryVar <- mkFlexVar
          let entryType = VarN entryVar
          let listType = AppN ModuleName.list N.list [entryType]

          (State headers vars revCons) <-
            foldM (addEntry region entryType) state (Index.indexedMap (,) patterns)

          let listCon = CPattern region PList listType expectation
          return $ State headers (entryVar:vars) (listCon:revCons)

    Can.PCons headPattern tailPattern ->
      do  entryVar <- mkFlexVar
          let entryType = VarN entryVar
          let listType = AppN ModuleName.list N.list [entryType]

          let headExpectation = NoPatternExpectation entryType
          let tailExpectation = PatternExpectation region PTail listType

          (State headers vars revCons) <-
            add headPattern headExpectation =<<
              add tailPattern tailExpectation state

          let listCon = CPattern region PList listType expectation
          return $ State headers (entryVar:vars) (listCon : revCons)

    Can.PRecord fields ->
      do  extVar <- mkFlexVar
          let extType = VarN extVar

          fieldVars <- traverse (\field -> (,) field <$> mkFlexVar) fields
          let fieldTypes = Map.fromList (map (fmap VarN) fieldVars)
          let recordType = RecordN fieldTypes extType

          let (State headers vars revCons) = state
          let recordCon = CPattern region PRecord recordType expectation
          return $
            State
              { _headers = Map.union headers (Map.map (A.A region) fieldTypes)
              , _vars = map snd fieldVars ++ extVar : vars
              , _revCons = recordCon : revCons
              }

    Can.PInt _ ->
      do  let (State headers vars revCons) = state
          let intCon = CPattern region PInt T.int expectation
          return $ State headers vars (intCon:revCons)

    Can.PStr _ ->
      do  let (State headers vars revCons) = state
          let strCon = CPattern region PStr T.string expectation
          return $ State headers vars (strCon:revCons)

    Can.PChr _ ->
      do  let (State headers vars revCons) = state
          let chrCon = CPattern region PChr T.char expectation
          return $ State headers vars (chrCon:revCons)



-- STATE HELPERS


{-# NOINLINE emptyState #-}
emptyState :: State
emptyState =
  State Map.empty [] []


addToHeaders :: R.Region -> N.Name -> PatternExpectation -> State -> State
addToHeaders region name expectation (State headers vars revCons) =
  let
    tipe = getType expectation
    newHeaders = Map.insert name (A.A region tipe) headers
  in
  State newHeaders vars revCons


getType :: PatternExpectation -> Type
getType expectation =
  case expectation of
    NoPatternExpectation tipe -> tipe
    PatternExpectation _ _ tipe -> tipe



-- CONSTRAIN LIST


addEntry :: R.Region -> Type -> State -> (Index.ZeroBased, Can.Pattern) -> IO State
addEntry listRegion tipe state (index, pattern) =
  let
    expectation =
      PatternExpectation listRegion (PListEntry index) tipe
  in
  add pattern expectation state



-- CONSTRAIN TUPLE


addTuple :: R.Region -> Can.Pattern -> Can.Pattern -> Maybe Can.Pattern -> PatternExpectation -> State -> IO State
addTuple region a b maybeC expectation state =
  do  aVar <- mkFlexVar
      bVar <- mkFlexVar
      let aType = VarN aVar
      let bType = VarN bVar

      case maybeC of
        Nothing ->
          do  (State headers vars revCons) <-
                simpleAdd b bType =<<
                  simpleAdd a aType state

              let tupleCon = CPattern region PTuple (TupleN aType bType Nothing) expectation

              return $ State headers (aVar:bVar:vars) (tupleCon:revCons)

        Just c ->
          do  cVar <- mkFlexVar
              let cType = VarN cVar

              (State headers vars revCons) <-
                simpleAdd c cType =<<
                  simpleAdd b bType =<<
                    simpleAdd a aType state

              let tupleCon = CPattern region PTuple (TupleN aType bType (Just cType)) expectation

              return $ State headers (aVar:bVar:cVar:vars) (tupleCon:revCons)


simpleAdd :: Can.Pattern -> Type -> State -> IO State
simpleAdd pattern patternType state =
  add pattern (NoPatternExpectation patternType) state



-- CONSTRAIN CONSTRUCTORS


addCtor :: R.Region -> ModuleName.Canonical -> N.Name -> [N.Name] -> N.Name -> [Can.PatternCtorArg] -> PatternExpectation -> State -> IO State
addCtor region home typeName typeVarNames ctorName args expectation state =
  do  varPairs <- traverse (\var -> (,) var <$> nameToFlex var) typeVarNames
      let typePairs = map (second VarN) varPairs
      let freeVarDict = Map.fromList typePairs

      (State headers vars revCons) <-
        foldM (addCtorArg region ctorName freeVarDict) state args

      let ctorType = AppN home typeName (map snd typePairs)
      let ctorCon = CPattern region (PCtor ctorName) ctorType expectation

      return $
        State
          { _headers = headers
          , _vars = map snd varPairs ++ vars
          , _revCons = ctorCon : revCons
          }


addCtorArg :: R.Region -> N.Name -> Map.Map N.Name Type -> State -> Can.PatternCtorArg -> IO State
addCtorArg region ctorName freeVarDict state (Can.PatternCtorArg index srcType pattern) =
  do  tipe <- Instantiate.fromSrcType freeVarDict srcType
      let expectation = PatternExpectation region (PCtorArg ctorName index) tipe
      add pattern expectation state
