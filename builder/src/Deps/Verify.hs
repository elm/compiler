module Deps.Verify
  ( verify
  )
  where


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Monad (filterM, void)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Set as Set
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

import qualified Deps.Cache as Cache
import qualified Deps.Explorer as Explorer
import qualified Deps.Solver as Solver
import qualified Deps.Website as Website
import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Objects as Obj
import qualified Elm.Constraint as Con
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.PerUserCache as PerUserCache
import Elm.Project.Json (Project(..), AppInfo(..), PkgInfo(..))
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified Elm.Version as V
import qualified File.Args as Args
import qualified File.Compile as Compile
import qualified File.Crawl as Crawl
import qualified File.IO as IO
import qualified File.Plan as Plan
import qualified Generate.Artifacts as Artifacts
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Deps as E
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task
import qualified Stuff.Paths as Paths



-- VERIFY


verify :: FilePath -> Project -> Task.Task Summary.Summary
verify root project =
  do  solution <-
        case project of
          Project.App info -> verifyApp info
          Project.Pkg info -> verifyPkg info

      (RawInfo infos ifaces) <- verifyArtifacts solution
      return (Summary.init root project infos ifaces)


throw :: E.Exit -> Task.Task a
throw exit =
  Task.throw (Exit.Deps exit)



-- VERIFY APP


verifyApp :: AppInfo -> Task.Task (Map.Map Pkg.Name V.Version)
verifyApp info =
  if _app_elm_version info /= V.compiler then
    throw (E.AppBadElm (_app_elm_version info))

  else
    do  oldSolution <- appToSolution info
        let solver = Solver.solve (Map.map Con.exactly oldSolution)
        registry <- Cache.optionalUpdate
        maybeSolution <- Explorer.run registry (Solver.run solver)
        case maybeSolution of
          Nothing ->
            throw E.BadDeps

          Just newSolution ->
            if Map.size oldSolution == Map.size newSolution then
              return newSolution
            else
              throw (E.AppMissingTrans (Map.toList (Map.difference newSolution oldSolution)))



-- VERIFY PKG


verifyPkg :: PkgInfo -> Task.Task (Map.Map Pkg.Name V.Version)
verifyPkg info =
  if not (Con.goodElm (_pkg_elm_version info)) then
    throw (E.PkgBadElm (_pkg_elm_version info))

  else
    do  deps <- union noDups (_pkg_deps info) (_pkg_test_deps info)
        let solver = Solver.solve deps
        registry <- Cache.optionalUpdate
        maybeSolution <- Explorer.run registry (Solver.run solver)
        case maybeSolution of
          Nothing ->
            throw E.BadDeps

          Just solution ->
            return solution



-- APP TO SOLUTION


appToSolution :: Project.AppInfo -> Task.Task (Map.Map Pkg.Name V.Version)
appToSolution (Project.AppInfo _ _ depsDirect depsTrans testDirect testTrans) =
  do  a <- union allowEqualDups depsTrans testDirect
      b <- union noDups depsDirect testTrans
      union noDups a b


noDups :: Pkg.Name -> a -> a -> Task.Task a
noDups _ _ _ =
  throw E.BadDeps


allowEqualDups :: Pkg.Name -> V.Version -> V.Version -> Task.Task V.Version
allowEqualDups _ v1 v2 =
  if v1 == v2 then
    return v1
  else
    throw E.BadDeps


union :: (Pkg.Name -> a -> a -> Task.Task a) -> Map.Map Pkg.Name a -> Map.Map Pkg.Name a -> Task.Task (Map.Map Pkg.Name a)
union tieBreaker deps1 deps2 =
  Map.mergeA Map.preserveMissing Map.preserveMissing (Map.zipWithAMatched tieBreaker) deps1 deps2



-- VERIFY ARTIFACTS


verifyArtifacts :: Map.Map Pkg.Name V.Version -> Task.Task RawInfo
verifyArtifacts solution =
  do  Website.download =<< filterM noSrc (Map.toList solution)
      verifyBuildArtifacts solution


noSrc :: (Pkg.Name, V.Version) -> Task.Task Bool
noSrc (name, version) =
  do  root <- Task.getPackageCacheDirFor name version
      liftIO $ not <$> doesDirectoryExist (root </> "src")



-- VERIFY BUILD ARTIFACTS


verifyBuildArtifacts :: Map.Map Pkg.Name V.Version -> Task.Task RawInfo
verifyBuildArtifacts solution =
  do  Task.report (Progress.BuildDepsStart (Map.size solution))
      startMVar <- liftIO newEmptyMVar
      ifacesMVar <- liftIO $ newMVar Map.empty
      pkgInfoMVars <- Map.traverseWithKey (verifyBuild startMVar ifacesMVar) solution
      liftIO $ putMVar startMVar pkgInfoMVars
      answers <- liftIO $ traverse readMVar pkgInfoMVars
      Task.report Progress.BuildDepsEnd
      RawInfo
        <$> Map.traverseMaybeWithKey toInfo answers
        <*> liftIO (readMVar ifacesMVar)


data RawInfo =
  RawInfo (Map.Map Pkg.Name PkgInfo) I.Interfaces


verifyBuild
  :: MVar (Map.Map Pkg.Name (MVar Answer))
  -> MVar I.Interfaces
  -> Pkg.Name
  -> V.Version
  -> Task.Task (MVar Answer)
verifyBuild pkgInfoMVar ifacesMVar name version =
  do  mvar <- liftIO newEmptyMVar
      info <- Cache.getElmJson name version
      report <- Task.getReporter
      runner <- Task.getSilentRunner

      liftIO $ void $ forkIO $
        do  allMVars <- readMVar pkgInfoMVar
            let deps = Project._pkg_deps info
            let depsMVars = Map.intersection allMVars deps
            depAnswers <- traverse readMVar depsMVars

            answer <- ifNotBlocked depAnswers $ \infos ->
              do  ifacesBefore <- readMVar ifacesMVar
                  result <- runner (getIface name version info infos ifacesBefore)
                  case result of
                    Right ifaces ->
                      do  ifacesNow <- takeMVar ifacesMVar
                          putMVar ifacesMVar (Map.union ifacesNow ifaces)
                          return (Ok info)

                    Left _ ->
                      return (Err name version)

            report Progress.BuildDepsProgress
            putMVar mvar answer

      return mvar



-- ANSWERS


data Answer
  = Ok PkgInfo
  | Blocked
  | Err Pkg.Name V.Version


toInfo :: Pkg.Name -> Answer -> Task.Task (Maybe PkgInfo)
toInfo _ answer =
  case answer of
    Ok info ->
      return (Just info)

    Blocked ->
      return Nothing

    Err name version ->
      do  elmHome <- liftIO PerUserCache.getElmHome
          throw (E.BuildFailure elmHome name version)


ifNotBlocked :: Map.Map Pkg.Name Answer -> (Map.Map Pkg.Name PkgInfo -> IO Answer) -> IO Answer
ifNotBlocked answers callback =
  case traverse isOk answers of
    Nothing ->
      return Blocked

    Just infos ->
      callback infos


isOk :: Answer -> Maybe PkgInfo
isOk answer =
  case answer of
    Ok info -> Just info
    Blocked -> Nothing
    Err _ _ -> Nothing



-- GET INTERFACE


getIface
  :: Pkg.Name
  -> V.Version
  -> PkgInfo
  -> Map.Map Pkg.Name PkgInfo
  -> I.Interfaces
  -> Task.Task I.Interfaces
getIface name version info infos depIfaces =
  do  root <- Task.getPackageCacheDirFor name version
      let solution = Map.map _pkg_version infos

      cached <- liftIO $ isCached root solution

      if cached
        then
          do  maybeIfaces <- liftIO $ IO.readBinary (root </> "ifaces.dat")
              maybe (Task.throw (error "TODO corrupt binary 1")) return maybeIfaces
        else
          do  Paths.removeStuff root
              let summary = Summary.cheapInit root info infos depIfaces
              args <- Args.fromSummary summary
              graph <- Crawl.crawl summary args
              (dirty, cachedIfaces) <- Plan.plan summary graph
              answers <- Compile.compile (Pkg info) cachedIfaces dirty
              results <- Artifacts.ignore answers
              Paths.removeStuff root

              updateCache root name info solution graph results



-- IS CACHED?


isCached :: FilePath -> Map.Map Pkg.Name V.Version -> IO Bool
isCached root solution =
  IO.andM
    [ IO.exists (root </> "cached.dat")
    , IO.exists (root </> "ifaces.dat")
    , IO.exists (root </> "objs.dat")
    , isCachedHelp solution <$> IO.readBinary (root </> "cached.dat")
    ]


isCachedHelp :: Map.Map Pkg.Name V.Version -> Maybe (Map.Map Pkg.Name (Set.Set V.Version)) -> Bool
isCachedHelp solution maybeCachedDeps =
  case maybeCachedDeps of
    Nothing ->
      False

    Just cachedDeps ->
      let
        matches =
          Map.intersectionWith Set.member solution cachedDeps
      in
      Map.size solution == Map.size matches
      && Map.foldr (&&) True matches



-- UPDATE CACHE


updateCache
  :: FilePath
  -> Pkg.Name
  -> PkgInfo
  -> Map.Map Pkg.Name V.Version
  -> Crawl.Result
  -> Map.Map ModuleName.Raw Compiler.Artifacts
  -> Task.Task I.Interfaces
updateCache root name info solution graph results =
  do  let path = root </> "cached.dat"
      let deps = Map.map Set.singleton solution
      let elmi = crush name info results

      exists <- liftIO $ IO.exists path

      if exists
        then
          do  maybeOldDeps <- liftIO $ IO.readBinary path
              oldDeps <- maybe (error "TODO corrupt binary 2") return maybeOldDeps
              liftIO $ IO.writeBinary path (Map.unionWith Set.union deps oldDeps)
        else
          liftIO $
          do  IO.writeBinary (root </> "ifaces.dat") elmi
              IO.writeBinary path deps
              IO.writeBinary (root </> "objs.dat") $
                Map.foldr addGraph (objGraphFromKernels graph) results

      return elmi



-- CRUSH INTERFACES


crush :: Pkg.Name -> PkgInfo -> Map.Map ModuleName.Raw Compiler.Artifacts -> I.Interfaces
crush pkg info results =
  let
    exposed =
      Set.fromList (Project.getExposed info)
  in
    Map.mapKeys (ModuleName.Canonical pkg) $
      Map.mapMaybeWithKey (crushHelp exposed) results


crushHelp :: Set.Set ModuleName.Raw -> ModuleName.Raw -> Compiler.Artifacts -> Maybe I.Interface
crushHelp exposed name (Compiler.Artifacts elmi _) =
  if Set.member name exposed then
    Just elmi

  else
    Nothing



-- OBJECT GRAPH


addGraph :: Compiler.Artifacts -> Obj.Graph -> Obj.Graph
addGraph (Compiler.Artifacts _ elmo) graph =
  Obj.union elmo graph


objGraphFromKernels :: Crawl.Result -> Obj.Graph
objGraphFromKernels (Crawl.Graph _ _ kernels _ _) =
  Obj.fromKernels kernels
