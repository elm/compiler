{-# OPTIONS_GHC -Wall #-}
module File.Args
  ( Args(..)
  , fromPaths
  , fromSummary
  )
  where


import qualified Elm.Compiler.Module as Module
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Make as E
import qualified Reporting.Task as Task


data Args root
  = Pkg [Module.Raw]
  | Roots root [root]


fromPaths :: Summary.Summary -> [FilePath] -> Task.Task (Args FilePath)
fromPaths summary paths =
  case paths of
    [] ->
      fromSummary summary

    first : rest ->
      return $ Roots first rest


fromSummary :: Summary.Summary -> Task.Task (Args a)
fromSummary (Summary.Summary _ project _ _ _) =
  case project of
    Project.Pkg info ->
      return $ Pkg (Project.getExposed info)

    Project.App _ ->
      Task.throw (Exit.Make E.CannotMakeNothing)
