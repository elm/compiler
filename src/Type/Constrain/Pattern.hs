{-# OPTIONS_GHC -Wall #-}
module Type.Constrain.Pattern
  ( State(..)
  , emptyState
  , addConstraints
  )
  where


import Control.Arrow (second)
import Control.Monad (foldM)
import qualified Data.Map.Strict as Map

import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified Data.Bag as Bag
import qualified Data.Index as Index
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Reporting.Region as R
import Type.Constraint
import qualified Type.Instantiate as Instantiate
import Type.Type as T



-- ADD CONSTRAINTS


-- The constraints are stored in reverse order so that adding a new
-- constraint is O(1) and we can reverse it at some later time.
--
data State =
  State
    { _headers :: Map.Map N.Name (A.Located Type)
    , _vars :: Bag.Bag Variable
    , _revCons :: [Constraint]
    }


emptyState :: State
emptyState =
  State Map.empty Bag.empty []


addConstraints :: R.Region -> Error.PatternContext -> Can.Pattern -> Type -> State -> IO State
addConstraints outerRegion context (A.A region pattern) tipe state =
  case pattern of
    Can.PAnything ->
      return state

    Can.PVar name ->
      do  let (State headers vars revCons) = state
          let newHeaders = Map.insert name (A.A region tipe) headers
          return $ State newHeaders vars revCons

    Can.PAlias realPattern name ->
      do  let (State headers vars revCons) = state
          addConstraints outerRegion context realPattern tipe $
            State (Map.insert name (A.A region tipe) headers) vars revCons

    Can.PUnit ->
      do  let (State headers vars revCons) = state
          let unitCon = CPattern outerRegion context Error.PUnit region UnitN tipe
          return $ State headers vars (unitCon:revCons)

    Can.PTuple a b maybeC ->
      addTupleConstraint outerRegion context region a b maybeC tipe state

    Can.PCtor home typeName typeVars ctorName args ->
      addCtorConstraint outerRegion context region home typeName typeVars ctorName args tipe state

    Can.PList patterns ->
      do  entryVar <- mkFlexVar
          let entryVarN = VarN entryVar
          let listType = AppN ModuleName.list N.list [entryVarN]

          (State headers vars revCons) <-
            foldM (addListConstraint region entryVarN) state (Index.indexedMap (,) patterns)

          let listCon = CPattern outerRegion context Error.PList region listType tipe
          return $ State headers (Bag.insert entryVar vars) (listCon:revCons)

    Can.PCons headPattern tailPattern ->
      do  entryVar <- mkFlexVar
          let entryType = VarN entryVar
          let listType = AppN ModuleName.list N.list [entryType]

          (State headers vars revCons) <-
            addConstraints region Error.PatternHead headPattern entryType =<<
              addConstraints region Error.PatternTail tailPattern listType state

          let listCon = CPattern outerRegion context Error.PList region listType tipe
          return $ State headers (Bag.insert entryVar vars) (listCon : revCons)

    Can.PRecord fields ->
      do  extVar <- mkFlexVar
          let extType = VarN extVar

          fieldVars <- traverse (\field -> (,) field <$> mkFlexVar) fields
          let fieldTypes = Map.fromList (map (fmap VarN) fieldVars)
          let recordType = RecordN fieldTypes extType

          let (State headers vars revCons) = state
          let recordCon = CPattern outerRegion context Error.PRecord region recordType tipe
          return $
            State
              { _headers = Map.union headers (Map.map (A.A region) fieldTypes)
              , _vars = Bag.insert extVar (Bag.append vars (Bag.fromList snd fieldVars))
              , _revCons = recordCon : revCons
              }

    Can.PInt _ ->
      do  let (State headers vars revCons) = state
          let intCon = CPattern outerRegion context Error.PInt region T.int tipe
          return $ State headers vars (intCon:revCons)

    Can.PStr _ ->
      do  let (State headers vars revCons) = state
          let strCon = CPattern outerRegion context Error.PStr region T.string tipe
          return $ State headers vars (strCon:revCons)

    Can.PChr _ ->
      do  let (State headers vars revCons) = state
          let chrCon = CPattern outerRegion context Error.PChr region T.char tipe
          return $ State headers vars (chrCon:revCons)





-- CONSTRAIN LIST


addListConstraint :: R.Region -> Type -> State -> (Index.ZeroBased, Can.Pattern) -> IO State
addListConstraint listRegion tipe state (index, pattern) =
  addConstraints listRegion (Error.PatternList index) pattern tipe state



-- CONSTRAIN TUPLE


addTupleConstraint
  :: R.Region
  -> Error.PatternContext
  -> R.Region
  -> Can.Pattern
  -> Can.Pattern
  -> Maybe Can.Pattern
  -> Type
  -> State
  -> IO State
addTupleConstraint parentRegion context tupleRegion a b maybeC tipe (State headers vars revCons) =
  do  aVar <- mkFlexVar
      bVar <- mkFlexVar
      let aVarN = VarN aVar
      let bVarN = VarN bVar

      let equal = CPattern parentRegion context Error.PTuple tupleRegion
      let addCon = addConstraints tupleRegion Error.PatternUnknown

      case maybeC of
        Nothing ->
          do  let tupleCon = equal (TupleN aVarN bVarN Nothing) tipe
              let newVars = Bag.insert aVar (Bag.insert bVar vars)
              addCon b bVarN =<<
                addCon a aVarN (State headers newVars (tupleCon:revCons))

        Just c ->
          do  cVar <- mkFlexVar
              let cVarN = VarN cVar
              let tupleCon = equal (TupleN aVarN bVarN (Just cVarN)) tipe
              let newVars = Bag.insert aVar (Bag.insert bVar (Bag.insert cVar vars))
              addCon c cVarN =<<
                addCon b bVarN =<<
                  addCon a aVarN (State headers newVars (tupleCon:revCons))



-- CONSTRAIN CONSTRUCTORS


addCtorConstraint
  :: R.Region
  -> Error.PatternContext
  -> R.Region
  -> ModuleName.Canonical
  -> N.Name
  -> [N.Name]
  -> N.Name
  -> [Can.PatternCtorArg]
  -> Type
  -> State
  -> IO State
addCtorConstraint outerRegion context ctorRegion home typeName typeVarNames ctorName args tipe state =
  do  varPairs <- traverse (\var -> (,) var <$> nameToFlex var) typeVarNames
      let typePairs = map (second VarN) varPairs
      let freeVarDict = Map.fromList typePairs

      (State headers vars revCons) <-
        foldM (addCtorArgConstraint ctorRegion ctorName freeVarDict) state args

      let ctorType = AppN home typeName (map snd typePairs)
      let ctorCon = CPattern outerRegion context (Error.PCtor ctorName) ctorRegion ctorType tipe

      return $
        State
          { _headers = headers
          , _vars = Bag.append vars (Bag.fromList snd varPairs)
          , _revCons = ctorCon : revCons
          }


addCtorArgConstraint :: R.Region -> N.Name -> Map.Map N.Name Type -> State -> Can.PatternCtorArg -> IO State
addCtorArgConstraint ctorRegion ctorName freeVarDict state (Can.PatternCtorArg index srcType pattern) =
  do  tipe <- Instantiate.fromSrcType freeVarDict srcType
      addConstraints ctorRegion (Error.PatternCtorArg ctorName index) pattern tipe state
