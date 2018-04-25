{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module File.Crawl
  ( Result
  , Graph(..)
  , crawl
  , crawlFromSource
  )
  where

import Control.Concurrent.Chan (Chan, newChan, readChan)
import Control.Monad (foldM)
import Control.Monad.Except (liftIO)
import qualified Data.ByteString as BS
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Objects as Obj
import qualified Elm.Kernel as Kernel
import qualified Elm.Package as Pkg

import qualified Elm.Project.Json as Project
import Elm.Project.Summary (Summary(..))
import qualified File.Args as Args
import qualified File.Find as Find
import qualified File.Header as Header
import qualified File.IO as IO
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Crawl as E
import qualified Reporting.Task as Task



-- GRAPH


data Graph kernel problems =
  Graph
    { _args :: Args.Args Module.Raw
    , _locals :: Map.Map Module.Raw Header.Info
    , _kernels :: Map.Map Module.Raw kernel
    , _foreigns :: Map.Map Module.Raw Pkg.Package
    , _problems :: problems
    }


type Result =
  Graph Obj.Kernel ()


type WorkGraph =
  Graph (FilePath, Maybe FilePath) [E.Problem]


initWorkGraph :: Args.Args Module.Raw -> Map.Map Module.Raw Header.Info -> WorkGraph
initWorkGraph args locals =
  Graph args locals Map.empty Map.empty []



-- CRAWL


crawl :: Summary -> Args.Args FilePath -> Task.Task Result
crawl summary args =
  case args of
    Args.Pkg names ->
      do  let roots = map (Unvisited E.ElmJson) names
          let graph = initWorkGraph (Args.Pkg names) Map.empty
          depthFirstSearch summary roots graph

    Args.Roots path [] ->
      crawlHelp summary =<< Header.readOneFile summary path

    Args.Roots p ps ->
      do  (h,hs) <- Header.readManyFiles summary p ps
          let unvisited = concatMap toUnvisited (h:hs)
          let graph = initWorkGraph (Args.Roots (fst h) (map fst hs)) (Map.fromList (h:hs))
          depthFirstSearch summary unvisited graph
      where
        toUnvisited (name, Header.Info path _ _ deps) =
          map (Unvisited (E.Module path name)) deps


crawlFromSource :: Summary -> BS.ByteString -> Task.Task Result
crawlFromSource summary@(Summary _ project _ _ _) source =
  crawlHelp summary =<<
    Header.readSource project source


crawlHelp :: Summary -> ( Maybe Module.Raw, Header.Info ) -> Task.Task Result
crawlHelp summary ( maybeName, info@(Header.Info path _ _ deps) ) =
  do  let name = maybe "Main" id maybeName
      let toUnvisited dep = Unvisited (maybe (E.File path) (E.Module path) maybeName) dep
      let roots = map toUnvisited deps
      let graph = initWorkGraph (Args.Roots name []) (Map.singleton name info)
      depthFirstSearch summary roots graph



-- DEPTH FIRST SEARCH


depthFirstSearch :: Summary -> [Unvisited] -> WorkGraph -> Task.Task Result
depthFirstSearch summary unvisited startGraph =
  do  chan <- liftIO newChan

      (Graph args locals kernelPaths foreigns problems) <-
        dfs summary chan 0 Set.empty unvisited startGraph

      case problems of
        [] ->
          do  checkForCycles locals
              kernels <- visitKernels summary locals kernelPaths
              return $ Graph args locals kernels foreigns ()

        problem : otherProblems ->
          Task.throw (Exit.Crawl (E.DependencyProblems problem otherProblems))



-- CHECK FOR CYCLES


checkForCycles :: Map.Map Module.Raw Header.Info -> Task.Task ()
checkForCycles locals =
  let
    toNode (name, Header.Info _ _ _ imports) =
      (name, name, imports)

    components =
      Graph.stronglyConnComp (map toNode (Map.toList locals))
  in
    mapM_ checkComponent components


checkComponent :: Graph.SCC Module.Raw -> Task.Task ()
checkComponent scc =
  case scc of
    Graph.AcyclicSCC _ ->
      return ()

    Graph.CyclicSCC names ->
      Task.throw (Exit.Cycle names)



-- CONCURRENT DEPTH FIRST SEARCH


dfs
  :: Summary
  -> Chan (Either E.Problem Asset)
  -> Int
  -> Set.Set Module.Raw
  -> [Unvisited]
  -> WorkGraph
  -> Task.Task WorkGraph
dfs summary chan oldPending oldSeen unvisited graph =
  do  (seen, pending) <-
        foldM (spawnVisitor summary chan) (oldSeen, oldPending) unvisited

      if pending == 0
        then return graph
        else
          do  asset <- liftIO $ readChan chan
              case asset of
                Right (Local name info@(Header.Info path _ _ imports)) ->
                  do  let newGraph = graph { _locals = Map.insert name info (_locals graph) }
                      let deps = map (Unvisited (E.Module path name)) imports
                      dfs summary chan (pending - 1) seen deps newGraph

                Right (Kernel name info) ->
                  do  let kernels = Map.insert name info (_kernels graph)
                      let newGraph = graph { _kernels = kernels }
                      dfs summary chan (pending - 1) seen [] newGraph

                Right (Foreign name pkg) ->
                  do  let foreigns = Map.insert name pkg (_foreigns graph)
                      let newGraph = graph { _foreigns = foreigns }
                      dfs summary chan (pending - 1) seen [] newGraph

                Right ForeignKernel ->
                  dfs summary chan (pending - 1) seen [] graph

                Left problem ->
                  do  let problems = problem : _problems graph
                      let newGraph = graph { _problems = problems }
                      dfs summary chan (pending - 1) seen [] newGraph


spawnVisitor
  :: Summary
  -> Chan (Either E.Problem Asset)
  -> (Set.Set Module.Raw, Int)
  -> Unvisited
  -> Task.Task (Set.Set Module.Raw, Int)
spawnVisitor summary chan (seen, n) unvisited@(Unvisited _ name) =
  if Set.member name seen then
    return (seen, n)
  else
    do  Task.workerChan chan (toVisitor summary unvisited)
        return (Set.insert name seen, n + 1)



-- VISITOR


data Unvisited =
  Unvisited
    { _origin :: E.Origin
    , _name :: Module.Raw
    }


data Asset
  = Local Module.Raw Header.Info
  | Kernel Module.Raw (FilePath, Maybe FilePath)
  | Foreign Module.Raw Pkg.Package
  | ForeignKernel


toVisitor :: Summary -> Unvisited -> Task.Task_ E.Problem Asset
toVisitor summary (Unvisited origin name) =
  do  asset <- Find.find summary origin name
      case asset of
        Find.Local path ->
          uncurry Local <$> Header.readModule summary name path

        Find.Foreign pkg ->
          return $ Foreign name pkg

        Find.Kernel clientPath maybeServerPath ->
          return $ Kernel name (clientPath, maybeServerPath)

        Find.ForeignKernel ->
          return ForeignKernel



-- VISIT KERNELS


visitKernels
  :: Summary
  -> Map.Map Module.Raw Header.Info
  -> Map.Map Module.Raw (FilePath, Maybe FilePath)
  -> Task.Task (Map.Map Module.Raw Obj.Kernel)
visitKernels summary locals kernelPaths =
  if Map.null kernelPaths then
    return Map.empty
  else
    -- TODO do this concurrently?
    -- Need better concurrency primitives in Task
    traverse (readKernel (toImportDict summary locals)) kernelPaths


readKernel :: ImportDict -> (FilePath, Maybe FilePath) -> Task.Task Obj.Kernel
readKernel importDict (clientPath, maybeServerPath) =
  Obj.Kernel
    <$> readKContent importDict clientPath
    <*> traverse (readKContent importDict) maybeServerPath


readKContent :: ImportDict -> FilePath -> Task.Task Kernel.KContent
readKContent importDict path =
  do  source <- liftIO $ IO.readUtf8 path
      case Kernel.parse importDict source of
        Right content ->
          return content

        Left _ ->
          Task.throw (Exit.Crawl (E.BadKernelHeader path))



-- IMPORT DICT


type ImportDict =
  Map.Map Module.Raw [Pkg.Name]


toImportDict :: Summary -> Map.Map Module.Raw info -> ImportDict
toImportDict (Summary _ project exposed _ _) locals =
  let
    localPkg =
      [Project.getName project]

    addLocal name _ dict =
      Map.insertWith (++) name localPkg dict
  in
  Map.foldrWithKey addLocal (Map.map (map Pkg._name) exposed) locals

