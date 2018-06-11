{-# OPTIONS_GHC -Wall #-}
module Stuff.Verify
  ( verify
  )
  where


import Control.Monad (liftM2, liftM4)
import Data.Binary
import Data.Map (Map)

import qualified Elm.Compiler.Module as Module
import Elm.Package (Name, Version)

import qualified Deps.Verify as Verify
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Constraint as Con
import qualified Elm.Project.Summary as Summary
import qualified File.IO as IO
import qualified Reporting.Task as Task
import qualified Stuff.Paths as Path



-- VERIFY


verify :: FilePath -> Project.Project -> Task.Task Summary.Summary
verify root project =
  do  exists <- IO.exists Path.summary
      if exists
        then
          do  (MiniSummary deps exposed ifaces graph) <- IO.readBinary Path.summary
              if isValidMiniSummary deps project
                then
                  return (Summary.Summary root project exposed ifaces graph)
                else
                  do  IO.remove Path.summary
                      cacheSummary root project
        else
          cacheSummary root project


cacheSummary :: FilePath -> Project.Project -> Task.Task Summary.Summary
cacheSummary root project =
  do  summary@(Summary.Summary _ _ exposed ifaces graph) <- Verify.verify root project
      IO.writeBinary Path.summary (MiniSummary (getDeps project) exposed ifaces graph)
      return summary



-- MINI SUMMARY


data MiniSummary =
  MiniSummary
    { _deps :: ProjectDeps
    , _exposed :: Summary.ExposedModules
    , _ifaces :: Module.Interfaces
    , _depsGraph :: Summary.DepsGraph
    }


data ProjectDeps
  = App (Map Name Version) (Map Name Version) (Map Name Version) (Map Name Version)
  | Pkg (Map Name Con.Constraint) (Map Name Con.Constraint)


getDeps :: Project.Project -> ProjectDeps
getDeps project =
  case project of
    Project.App (Project.AppInfo _ _ depsDirect depsTrans testDirect testTrans) ->
      App depsDirect depsTrans testDirect testTrans

    Project.Pkg (Project.PkgInfo _ _ _ _ _ direct test _) ->
      Pkg direct test


isValidMiniSummary :: ProjectDeps -> Project.Project -> Bool
isValidMiniSummary deps project =
  case deps of
    App a b c d ->
      case project of
        Project.Pkg _ ->
          False

        Project.App (Project.AppInfo _ _ a2 b2 c2 d2) ->
          a == a2 && b == b2 && c == c2 && d == d2

    Pkg a b ->
      case project of
        Project.Pkg (Project.PkgInfo _ _ _ _ _ a2 b2 _) ->
          a == a2 && b == b2

        Project.App _ ->
          False



-- BINARY


instance Binary MiniSummary where
  get = liftM4 MiniSummary get get get get
  put (MiniSummary a b c d) = put a >> put b >> put c >> put d



instance Binary ProjectDeps where
  put deps =
    case deps of
      App a b c d -> putWord8 0 >> put a >> put b >> put c >> put d
      Pkg a b     -> putWord8 1 >> put a >> put b

  get =
    do  n <- getWord8
        case n of
          0 -> liftM4 App get get get get
          1 -> liftM2 Pkg get get
          _ -> error "binary encoding of ProjectDeps was corrupted"