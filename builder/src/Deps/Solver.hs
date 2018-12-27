module Deps.Solver
  ( Solver
  , run
  , solve
  )
  where


import Control.Monad (foldM, guard, mzero, msum)
import Control.Monad.Logic (LogicT, runLogicT, lift)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)

import Deps.Explorer (Explorer)
import qualified Deps.Explorer as Explorer
import qualified Elm.Constraint as C
import qualified Elm.Package as Pkg
import qualified Elm.Version as V



-- SOLVE


solve :: Map Pkg.Name C.Constraint -> Solver (Map Pkg.Name V.Version)
solve cons =
  mkSolver (State Map.empty cons)


type Solver a =
  LogicT Explorer a


run :: Solver a -> Explorer (Maybe a)
run solver =
  runLogicT solver (const . return . Just) (return Nothing)



-- SOLVER


data State =
  State
    { _solution :: Map Pkg.Name V.Version
    , _unsolved :: Map Pkg.Name C.Constraint
    }


mkSolver :: State -> Solver (Map Pkg.Name V.Version)
mkSolver (State solution unsolved) =
  case Map.minViewWithKey unsolved of
    Nothing ->
      return solution

    Just ((name, constraint), otherUnsolved) ->
      do  allVersions <- lift $ Explorer.getVersions name
          let versions =
                reverse $ List.sort $
                  filter (C.satisfies constraint) allVersions

          let state1 = State solution otherUnsolved
          state2 <- msum (map (addVersion state1 name) versions)
          mkSolver state2


addVersion :: State -> Pkg.Name -> V.Version -> Solver State
addVersion (State solution unsolved) name version =
  do  (Explorer.Info elm cons) <-
        lift $ Explorer.getConstraints name version

      guard (C.goodElm elm)
      newUnsolved <- foldM (addConstraint solution) unsolved (Map.toList cons)
      return (State (Map.insert name version solution) newUnsolved)


addConstraint :: Map Pkg.Name V.Version -> Map Pkg.Name C.Constraint -> (Pkg.Name, C.Constraint) -> Solver (Map Pkg.Name C.Constraint)
addConstraint solution unsolved (name, newConstraint) =
  case Map.lookup name solution of
    Just version ->
      if C.satisfies newConstraint version then
        return unsolved
      else
        mzero

    Nothing ->
      case Map.lookup name unsolved of
        Nothing ->
          return $ Map.insert name newConstraint unsolved

        Just oldConstraint ->
          case C.intersect oldConstraint newConstraint of
            Nothing ->
              mzero

            Just mergedConstraint ->
              if oldConstraint == mergedConstraint then
                return unsolved
              else
                return (Map.insert name mergedConstraint unsolved)

