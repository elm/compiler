{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Project
  ( getRoot
  , getRootWithReplFallback
  , compile
  , compileForRepl
  , generateDocs
  )
  where


import qualified Data.ByteString as BS
import System.FilePath ((</>))

import qualified Elm.Docs as Docs
import qualified Elm.Name as N
import qualified Elm.Project.Root as Root
import qualified Elm.Project.Summary as Summary
import Elm.Project.Summary (Summary)
import qualified File.Args as Args
import qualified File.Artifacts as Artifacts
import qualified File.Compile as Compile
import qualified File.Crawl as Crawl
import qualified File.Plan as Plan
import qualified Generate.Output as Output
import qualified Reporting.Task as Task
import qualified Stuff.Paths as Path



-- GET ROOT


getRoot :: Task.Task Summary
getRoot =
  Root.get


getRootWithReplFallback :: IO FilePath
getRootWithReplFallback =
  Root.getWithReplFallback



-- COMPILE


compile :: Output.Options -> Maybe FilePath -> Summary -> [FilePath] -> Task.Task ()
compile options docs summary@(Summary.Summary root project _ _ _) paths =
  do  args <- Args.fromPaths summary paths
      graph <- Crawl.crawl summary args
      (dirty, ifaces) <- Plan.plan docs summary graph
      answers <- Compile.compile project docs ifaces dirty
      results <- Artifacts.write root answers
      _ <- traverse (Artifacts.writeDocs results) docs
      Output.generate options summary graph



-- COMPILE FOR REPL


compileForRepl :: BS.ByteString -> Maybe N.Name -> Task.Task (Maybe FilePath)
compileForRepl source maybeName =
  do  summary@(Summary.Summary root project _ _ _) <- getRoot
      graph <- Crawl.crawlFromSource summary source
      (dirty, ifaces) <- Plan.plan Nothing summary graph
      answers <- Compile.compile project Nothing ifaces dirty
      _results <- Artifacts.write root answers
      traverse (Output.generateReplFile summary graph) maybeName



-- GENERATE DOCS


generateDocs :: Summary.Summary -> Task.Task Docs.Documentation
generateDocs summary@(Summary.Summary root project _ _ _) =
  do  let docsPath = root </> Path.docs
      args <- Args.fromSummary summary
      graph <- Crawl.crawl summary args
      (dirty, ifaces) <- Plan.plan (Just docsPath) summary graph
      answers <- Compile.compile project (Just docsPath) ifaces dirty
      results <- Artifacts.ignore answers
      Artifacts.writeDocs results docsPath
