{-# OPTIONS_GHC -Wall #-}
module Stuff.Verify
  ( verify
  )
  where


import Prelude hiding (read)
import qualified Data.Map as Map
import Data.Map (Map)

import Elm.Package (Name, Version)

import qualified Deps.Verify as Verify
import Elm.Project.Json (Project(..), PkgInfo(..))
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Constraint as Con
import qualified Elm.Project.Summary as Summary
import qualified File.IO as IO
import qualified Reporting.Task as Task
import qualified Stuff.Paths as Path



-- VERIFY


verify :: FilePath -> Project -> Task.Task Summary.Summary
verify root project =
  do  fresh <-
        IO.andM
          [ IO.exists Path.solution
          , IO.exists Path.summary
          , checkSolution project
          ]

      if fresh
        then read root project
        else rebuildCache root project


rebuildCache :: FilePath -> Project -> Task.Task Summary.Summary
rebuildCache root project =
  do  IO.remove Path.solution
      IO.remove Path.summary

      (solution, summary) <- Verify.verify root project

      IO.writeBinary Path.solution solution
      write summary
      return summary



-- READ / WRITE


read :: FilePath -> Project -> Task.Task Summary.Summary
read root project =
  do  (exposed, ifaces, graph) <- IO.readBinary Path.summary
      return (Summary.Summary root project exposed ifaces graph)


write :: Summary.Summary -> Task.Task ()
write (Summary.Summary _ _ exposed ifaces graph) =
  IO.writeBinary Path.summary ( exposed, ifaces, graph )



-- CHECK SOLUTION


checkSolution :: Project -> Task.Task Bool
checkSolution project =
  checkSolutionHelp project <$> IO.readBinary Path.solution


checkSolutionHelp :: Project -> Map Name Version -> Bool
checkSolutionHelp project solution =
  case project of
    App info ->
      solution == Project.appSolution info

    Pkg info ->
      allGood (_pkg_test_deps info) solution
      && allGood (_pkg_deps info) solution


allGood :: Map Name Con.Constraint -> Map Name Version -> Bool
allGood cons vsns =
  let
    bools =
      Map.intersectionWith Con.satisfies cons vsns
  in
    Map.size cons == Map.size bools
    && Map.foldr (&&) True bools
