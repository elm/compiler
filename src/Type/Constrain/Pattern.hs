{-# OPTIONS_GHC -Wall #-}
module Type.Constrain.Pattern
  ( Info(..)
  , constrain
  )
  where


import Control.Monad (foldM)
import qualified Data.Map.Strict as Map

import qualified AST.Expression.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified Data.Bag as Bag
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Reporting.Region as R
import Type.Constraint
import qualified Type.Constrain.Literal as Literal
import qualified Type.Instantiate as Instantiate
import Type.Type



-- CONSTRAIN PATTERN


constrain :: Can.Pattern -> Type -> IO (Map.Map N.Name (A.Located Type), [Variable], Constraint)
constrain pattern tipe =
  do  (Info headers vars cons) <-
        addConstraints Error.PatternUnknown pattern tipe $
          Info Map.empty Bag.empty []

      return ( headers, Bag.toList vars, CAnd (reverse cons) )



-- ADD CONSTRAINTS


data Info =
  Info
    { _headers :: Map.Map N.Name (A.Located Type)
    , _vars :: Bag.Bag Variable
    , _cons :: [Constraint]
    }


addConstraints :: Error.PatternContext -> Can.Pattern -> Type -> Info -> IO Info
addConstraints context (A.A region pattern) tipe info =
  case pattern of
    Can.PAnything ->
      return info

    Can.PLiteral lit ->
      do  let (Info headers vars revCons) = info
          litCon <- Literal.constrain region lit tipe
          return $ Info headers vars (litCon:revCons)

    Can.PVar name ->
      do  let (Info headers vars revCons) = info
          let newHeaders = Map.insert name (A.A region tipe) headers
          return $ Info newHeaders vars revCons

    Can.PAlias realPattern name ->
      do  let (Info headers vars revCons) = info
          addConstraints context realPattern tipe $
            Info (Map.insert name (A.A region tipe) headers) vars revCons

    Can.PUnit ->
      do  let (Info headers vars revCons) = info
          let unitCon = CEqual (Error.Pattern context Error.PUnit) region UnitN tipe
          return $ Info headers vars (unitCon:revCons)

    Can.PTuple a b maybeC ->
      addTupleConstraint context region a b maybeC tipe info

    Can.PCtor home typeName vs ctorName args ->
      do  (Instantiate.Ctor tvars targs ttype) <- Instantiate.pattern home typeName vs args

          (Info headers vars revCons) <-
            foldM (addCtorConstraint ctorName) info targs

          return $
            Info
              { _headers = headers
              , _vars = Bag.append vars (Bag.fromList id tvars)
              , _cons =
                  CEqual (Error.Pattern context (Error.PCtor ctorName)) region ttype tipe
                  : revCons
              }

    Can.PList patterns ->
      do  entryVar <- mkFlexVar
          let entryVarN = VarN entryVar
          let listType = AppN ModuleName.list N.list [entryVarN]

          (Info headers vars revCons) <-
            foldM
              (addListConstraint entryVarN)
              info
              (zip patterns [ 1 .. length patterns ])

          let listCon = CEqual (Error.Pattern context Error.PList) region listType tipe
          return $ Info headers (Bag.insert entryVar vars) (listCon:revCons)

    Can.PCons headPattern tailPattern ->
      do  headVar <- mkFlexVar
          tailVar <- mkFlexVar
          let headVarN = VarN headVar
          let tailVarN = VarN tailVar
          let tailType = AppN ModuleName.list N.list [tailVarN]

          (Info headers vars revCons) <-
            addConstraints Error.PatternTail tailPattern tailType =<<
              addConstraints Error.PatternUnknown headPattern headVarN info

          return $
            Info
              { _headers = headers
              , _vars = Bag.insert headVar (Bag.insert tailVar vars)
              , _cons =
                  CEqual (Error.Pattern context Error.PList) region tailType tipe
                  : CEqual Error.PatternCons region headVarN tailVarN
                  : revCons
              }

    Can.PRecord fields ->
      do  extVar <- mkFlexVar

          fieldVars <- traverse (\field -> (,) field <$> mkFlexVar) fields
          let fieldVarN = Map.fromList (map (fmap VarN) fieldVars)

          let (Info headers vars revCons) = info
          return $
            Info
              { _headers = Map.union headers (Map.map (A.A region) fieldVarN)
              , _vars = Bag.insert extVar (Bag.append vars (Bag.fromList snd fieldVars))
              , _cons =
                  CEqual
                    (Error.Pattern context Error.PRecord)
                    region
                    (RecordN fieldVarN (VarN extVar))
                    tipe
                  : revCons
              }



-- CONSTRAIN CONSTRUCTORS


addCtorConstraint :: N.Name -> Info -> (Int, Type, Can.Pattern) -> IO Info
addCtorConstraint name info (index, argType, argPattern) =
  addConstraints (Error.PatternArg name index) argPattern argType info



-- CONSTRAIN LIST


addListConstraint :: Type -> Info -> (Can.Pattern, Int) -> IO Info
addListConstraint tipe info (pattern, index) =
  addConstraints (Error.PatternList index) pattern tipe info



-- CONSTRAIN TUPLE


addTupleConstraint
  :: Error.PatternContext
  -> R.Region
  -> Can.Pattern
  -> Can.Pattern
  -> Maybe Can.Pattern
  -> Type
  -> Info
  -> IO Info
addTupleConstraint context region a b maybeC tipe (Info headers vars revCons) =
  do  let equal = CEqual (Error.Pattern context Error.PTuple) region
      let addCon = addConstraints Error.PatternUnknown

      aVar <- mkFlexVar
      bVar <- mkFlexVar
      let aVarN = VarN aVar
      let bVarN = VarN bVar

      case maybeC of
        Nothing ->
          do  let tupleCon = equal (TupleN aVarN bVarN Nothing) tipe
              let newVars = Bag.insert aVar (Bag.insert bVar vars)
              addCon b bVarN =<<
                addCon a aVarN (Info headers newVars (tupleCon:revCons))

        Just c ->
          do  cVar <- mkFlexVar
              let cVarN = VarN cVar
              let tupleCon = equal (TupleN aVarN bVarN (Just cVarN)) tipe
              let newVars = Bag.insert aVar (Bag.insert bVar (Bag.insert cVar vars))
              addCon c cVarN =<<
                addCon b bVarN =<<
                  addCon a aVarN (Info headers newVars (tupleCon:revCons))
