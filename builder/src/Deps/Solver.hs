{-# LANGUAGE OverloadedStrings, Rank2Types #-}
module Deps.Solver
  ( Solver
  , Details(..)
  , Connection(..)
  , solve
  )
  where


import Control.Monad (foldM)
import qualified Data.Map as Map
import qualified System.Directory as Dir

import qualified Deps.Registry as Registry
import qualified Deps.Website as Website
import qualified Elm.Constraint as C
import qualified Elm.Package as Pkg
import qualified Elm.Outline as Outline
import qualified Elm.Version as V
import qualified File
import qualified Http
import qualified Json.Decode as D
import qualified Stuff



-- SOLVER


newtype Solver a =
  Solver
  (
    forall b.
      State
      -> (State -> a -> (State -> IO b) -> IO b)
      -> (State -> IO b)
      -> IO b
  )


data State =
  State
    { _cache :: Stuff.PackageCache
    , _internet :: Connection
    , _registry :: Registry.Registry
    , _constraints :: Map.Map (Pkg.Name, V.Version) Constraints
    , _problems :: [Problem]
    }


data Constraints =
  Constraints
    { _elm :: C.Constraint
    , _deps :: Map.Map Pkg.Name C.Constraint
    }


data Connection
  = Online Http.Manager
  | Offline


data Problem
  = BadCacheData Pkg.Name V.Version
  | BadHttpData Pkg.Name V.Version
  | BadHttp Http.Error
  | UnknownPackage [Pkg.Name]
  | ImpossibleConstraint Pkg.Name C.Constraint
  | UnavailableOffline Pkg.Name V.Version



-- SOLVE


data Details =
  Details V.Version [Pkg.Name]


solve :: Stuff.PackageCache -> Connection -> Registry.Registry -> Map.Map Pkg.Name C.Constraint -> IO (Either [Problem] (Map.Map Pkg.Name Details))
solve cache connection registry constraints =
  let
    (Solver solver) = exploreGoals (Goals constraints Map.empty)
  in
  solver (State cache connection registry Map.empty [])
    (\(State _ _ _ cs _) vs _ -> return $ Right $ getDetails vs cs)
    (\(State _ _ _ _ ps) -> return $ Left ps)


getDetails :: Map.Map Pkg.Name V.Version -> Map.Map (Pkg.Name, V.Version) Constraints -> Map.Map Pkg.Name Details
getDetails solution constraints =
  Map.mapWithKey (getDetailsHelp constraints) solution


getDetailsHelp :: Map.Map (Pkg.Name, V.Version) Constraints -> Pkg.Name -> V.Version -> Details
getDetailsHelp constraints name vsn =
  case Map.lookup (name, vsn) constraints of
    Just (Constraints _ deps) ->
      Details vsn (Map.keys deps)

    Nothing ->
      error "compiler bug in Deps.Solver manifesting in getDetailsHelp"



-- EXPLORE GOALS


data Goals =
  Goals
    { _pending :: Map.Map Pkg.Name C.Constraint
    , _solved :: Map.Map Pkg.Name V.Version
    }


exploreGoals :: Goals -> Solver (Map.Map Pkg.Name V.Version)
exploreGoals (Goals pending solved) =
  case Map.minViewWithKey pending of
    Nothing ->
      return solved

    Just ((name, constraint), otherPending) ->
      do  let goals1 = Goals otherPending solved
          let addVsn = addVersion goals1 name
          (v,vs) <- getRelevantVersions name constraint
          goals2 <- oneOf (addVsn v) (map addVsn vs)
          exploreGoals goals2


addVersion :: Goals -> Pkg.Name -> V.Version -> Solver Goals
addVersion (Goals pending solved) name version =
  do  (Constraints elm deps) <- getConstraints name version
      if C.goodElm elm
        then
          do  newPending <- foldM (addConstraint solved) pending (Map.toList deps)
              return (Goals newPending (Map.insert name version solved))
        else
          deadend


addConstraint :: Map.Map Pkg.Name V.Version -> Map.Map Pkg.Name C.Constraint -> (Pkg.Name, C.Constraint) -> Solver (Map.Map Pkg.Name C.Constraint)
addConstraint solved unsolved (name, newConstraint) =
  case Map.lookup name solved of
    Just version ->
      if C.satisfies newConstraint version
      then return unsolved
      else deadend

    Nothing ->
      case Map.lookup name unsolved of
        Nothing ->
          return $ Map.insert name newConstraint unsolved

        Just oldConstraint ->
          case C.intersect oldConstraint newConstraint of
            Nothing ->
              deadend

            Just mergedConstraint ->
              if oldConstraint == mergedConstraint
              then return unsolved
              else return (Map.insert name mergedConstraint unsolved)



-- GET RELEVANT VERSIONS


getRelevantVersions :: Pkg.Name -> C.Constraint -> Solver (V.Version, [V.Version])
getRelevantVersions name constraint =
  Solver $ \state@(State _ _ registry _ _) ok err ->
    case Registry.getVersions' name registry of
      Right (Registry.KnownVersions newest previous) ->
        case filter (C.satisfies constraint) (newest:previous) of
          [] ->
            err (addProblem (ImpossibleConstraint name constraint) state)

          v:vs ->
            ok state (v,vs) err

      Left suggestions ->
        err (addProblem (UnknownPackage suggestions) state)



-- GET CONSTRAINTS


getConstraints :: Pkg.Name -> V.Version -> Solver Constraints
getConstraints name version =
  Solver $ \state@(State cache connection registry cDict ps) ok err ->
    do  let key = (name, version)
        case Map.lookup key cDict of
          Just cs ->
            ok state cs err

          Nothing ->
            do  result <- getConstraintsHelp cache connection name version
                case result of
                  Right cs ->
                    ok (State cache connection registry (Map.insert key cs cDict) ps) cs err

                  Left p ->
                    err (State cache connection registry cDict (p:ps))


getConstraintsHelp :: Stuff.PackageCache -> Connection -> Pkg.Name -> V.Version -> IO (Either Problem Constraints)
getConstraintsHelp cache connection name version =
  do  let path = Stuff.metadata cache name version "elm.json"
      outlineExists <- File.exists path
      if outlineExists
        then
          do  result <- D.fromFile constraintsDecoder path
              case result of
                Right cs ->
                  case connection of
                    Online _ ->
                      return $ Right cs

                    Offline ->
                      do  srcExists <- Dir.doesDirectoryExist (Stuff.metadata cache name version "src")
                          if srcExists
                            then return $ Right cs
                            else return $ Left (UnavailableOffline name version)

                Left  _  ->
                  do  File.remove path
                      return $ Left (BadCacheData name version)
        else
          case connection of
            Offline ->
              return (Left (UnavailableOffline name version))

            Online manager ->
              Http.post manager (Website.metadata name version "elm.json") [] BadHttp $ \body ->
                case D.fromByteString constraintsDecoder body of
                  Right cs ->
                    do  File.writeUtf8 path body
                        return $ Right cs

                  Left _ ->
                    return $ Left (BadHttpData name version)


constraintsDecoder :: D.Decoder () Constraints
constraintsDecoder =
  do  outline <- D.mapError (const ()) Outline.decoder
      case outline of
        Outline.Pkg (Outline.PkgOutline _ _ _ _ _ deps _ elmConstraint) ->
          return (Constraints elmConstraint deps)

        Outline.App _ ->
          D.failure ()



-- INSTANCES


instance Functor Solver where
  fmap func (Solver solver) =
    Solver $ \state ok err ->
      let
        okA stateA arg errA = ok stateA (func arg) errA
      in
      solver state okA err


instance Applicative Solver where
  pure a =
    Solver $ \state ok err -> ok state a err

  (<*>) (Solver solverFunc) (Solver solverArg) =
    Solver $ \state ok err ->
      let
        okF stateF func errF =
          let
            okA stateA arg errA = ok stateA (func arg) errA
          in
          solverArg stateF okA errF
      in
      solverFunc state okF err


instance Monad Solver where
  return a =
    Solver $ \state ok err -> ok state a err

  (>>=) (Solver solverA) callback =
    Solver $ \state ok err ->
      let
        okA stateA a errA =
          case callback a of
            Solver solverB -> solverB stateA ok errA
      in
      solverA state okA err


oneOf :: Solver a -> [Solver a] -> Solver a
oneOf solver@(Solver solverHead) solvers =
  case solvers of
    [] ->
      solver

    s:ss ->
      Solver $ \state0 ok err ->
        solverHead state0 ok $ \state1 ->
          let
            (Solver solverTail) = oneOf s ss
          in
          solverTail state1 ok err


deadend :: Solver a
deadend =
  Solver $ \state _ err -> err state


addProblem :: Problem -> State -> State
addProblem problem (State cache registry cDict maybeManager problems) =
  State cache registry cDict maybeManager (problem:problems)
