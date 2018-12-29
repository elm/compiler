{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Project
  ( getRoot
  , getRootWithReplFallback
  , compile
  , compileForRepl
  )
  where


import qualified Data.ByteString as BS
import Data.Map ((!))
import qualified Data.Name as Name

import qualified Elm.Compiler as Compiler
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Root as Root
import qualified Elm.Project.Summary as Summary
import Elm.Project.Summary (Summary)
import qualified File.Args as Args
import qualified File.Compile as Compile
import qualified File.Crawl as Crawl
import qualified File.Plan as Plan
import qualified Generate.Artifacts as Artifacts
import qualified Generate.Output as Output
import qualified Reporting.Render.Type.Localizer as L
import qualified Reporting.Task as Task



-- GET ROOT


getRoot :: Task.Task Summary
getRoot =
  Root.get


getRootWithReplFallback :: IO FilePath
getRootWithReplFallback =
  Root.getWithReplFallback



-- COMPILE


compile
  :: Output.Mode
  -> Output.Target
  -> Maybe Output.Output
  -> Summary
  -> [FilePath]
  -> Task.Task ()
compile mode target maybeOutput summary@(Summary.Summary root project _ _ _) paths =
  do  Project.check project
      args <- Args.fromPaths summary paths
      graph <- Crawl.crawl summary args
      (dirty, ifaces) <- Plan.plan summary graph
      answers <- Compile.compile project ifaces dirty
      results <- Artifacts.write root answers
      -- _ <- traverse (Artifacts.writeDocs results) docs
      Output.generate mode target maybeOutput summary graph results



-- COMPILE FOR REPL


compileForRepl :: Bool -> L.Localizer -> BS.ByteString -> Maybe Name.Name -> Task.Task (Maybe FilePath)
compileForRepl noColors localizer source maybeName =
  do  summary@(Summary.Summary root project _ _ _) <- getRoot
      Project.check project
      graph <- Crawl.crawlFromSource summary source
      (dirty, ifaces) <- Plan.plan summary graph
      answers <- Compile.compile project ifaces dirty
      results <- Artifacts.write root answers
      let (Compiler.Artifacts elmi _) = results ! Name.replModule
      traverse (Output.generateReplFile noColors localizer summary graph elmi) maybeName
