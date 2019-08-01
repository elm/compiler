{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Elm.Details
  ( Details(..)
  , BuildID
  , ValidOutline(..)
  , Local(..)
  , Foreign(..)
  , load
  , loadObjects
  , loadInterfaces
  , verifyInstall
  )
  where


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Monad (liftM, liftM2, liftM3)
import Data.Binary (Binary, get, put, getWord8, putWord8)
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Map.Utils as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE
import qualified Data.OneOrMore as OneOrMore
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8
import Data.Word (Word64)
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Optimized as Opt
import qualified BackgroundWriter as BW
import qualified Compile
import qualified Deps.Registry as Registry
import qualified Deps.Solver as Solver
import qualified Deps.Website as Website
import qualified Elm.Constraint as Con
import qualified Elm.Docs as Docs
import qualified Elm.Interface as I
import qualified Elm.Kernel as Kernel
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified File
import qualified Http
import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Parse.Module as Parse
import qualified Reporting
import qualified Reporting.Annotation as A
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff



-- DETAILS


data Details =
  Details
    { _outlineTime :: File.Time
    , _outline :: ValidOutline
    , _buildID :: BuildID
    , _locals :: Map.Map ModuleName.Raw Local
    , _foreigns :: Map.Map ModuleName.Raw Foreign
    , _extras :: Extras
    }


type BuildID = Word64


data ValidOutline
  = ValidApp (NE.List Outline.SrcDir)
  | ValidPkg Pkg.Name [ModuleName.Raw] (Map.Map Pkg.Name V.Version {- for docs in reactor -})


-- NOTE: we need two ways to detect if a file must be recompiled:
--
-- (1) _time is the modification time from the last time we compiled the file.
-- By checking EQUALITY with the current modification time, we can detect file
-- saves and `git checkout` of previous versions. Both need a recompile.
--
-- (2) _lastChange is the BuildID from the last time a new interface file was
-- generated, and _lastCompile is the BuildID from the last time the file was
-- compiled. These may be different if a file is recompiled but the interface
-- stayed the same. When the _lastCompile is LESS THAN the _lastChange of any
-- imports, we need to recompile. This can happen when a project has multiple
-- entrypoints and some modules are compiled less often than their imports.
--
data Local =
  Local
    { _path :: FilePath
    , _time :: File.Time
    , _deps :: [ModuleName.Raw]
    , _main :: Bool
    , _lastChange :: BuildID
    , _lastCompile :: BuildID
    }


data Foreign =
  Foreign Pkg.Name [Pkg.Name]


data Extras
  = ArtifactsCached
  | ArtifactsFresh Interfaces Opt.GlobalGraph


type Interfaces =
  Map.Map ModuleName.Canonical I.DependencyInterface



-- LOAD ARTIFACTS


loadObjects :: FilePath -> Details -> IO (MVar (Maybe Opt.GlobalGraph))
loadObjects root (Details _ _ _ _ _ extras) =
  case extras of
    ArtifactsFresh _ o -> newMVar (Just o)
    ArtifactsCached    -> fork (File.readBinary (Stuff.objects root))


loadInterfaces :: FilePath -> Details -> IO (MVar (Maybe Interfaces))
loadInterfaces root (Details _ _ _ _ _ extras) =
  case extras of
    ArtifactsFresh i _ -> newMVar (Just i)
    ArtifactsCached    -> fork (File.readBinary (Stuff.interfaces root))



-- VERIFY INSTALL -- used by Install


verifyInstall :: BW.Scope -> FilePath -> Solver.Env -> Outline.Outline -> IO (Either Exit.Details ())
verifyInstall scope root (Solver.Env cache manager connection registry) outline =
  do  time <- File.getTime (root </> "elm.json")
      let key = Reporting.ignorer
      let env = Env key scope root cache manager connection registry
      case outline of
        Outline.Pkg pkg -> Task.run (verifyPkg env time pkg >> return ())
        Outline.App app -> Task.run (verifyApp env time app >> return ())



-- LOAD -- used by Make, Repl, Reactor


load :: Reporting.Style -> BW.Scope -> FilePath -> IO (Either Exit.Details Details)
load style scope root =
  do  newTime <- File.getTime (root </> "elm.json")
      maybeDetails <- File.readBinary (Stuff.details root)
      case maybeDetails of
        Nothing ->
          generate style scope root newTime

        Just details@(Details oldTime _ buildID _ _ _) ->
          if oldTime == newTime
          then return (Right details { _buildID = buildID + 1 })
          else generate style scope root newTime



-- GENERATE


generate :: Reporting.Style -> BW.Scope -> FilePath -> File.Time -> IO (Either Exit.Details Details)
generate style scope root time =
  Reporting.trackDetails style $ \key ->
    do  result <- initEnv key scope root
        case result of
          Left exit ->
            return (Left exit)

          Right (env, outline) ->
            case outline of
              Outline.Pkg pkg -> Task.run (verifyPkg env time pkg)
              Outline.App app -> Task.run (verifyApp env time app)



-- ENV


data Env =
  Env
    { _key :: Reporting.DKey
    , _scope :: BW.Scope
    , _root :: FilePath
    , _cache :: Stuff.PackageCache
    , _manager :: Http.Manager
    , _connection :: Solver.Connection
    , _registry :: Registry.Registry
    }


initEnv :: Reporting.DKey -> BW.Scope -> FilePath -> IO (Either Exit.Details (Env, Outline.Outline))
initEnv key scope root =
  do  mvar <- fork Solver.initEnv
      eitherOutline <- Outline.read root
      case eitherOutline of
        Left problem ->
          return $ Left $ Exit.DetailsBadOutline problem

        Right outline ->
          do  maybeEnv <- readMVar mvar
              case maybeEnv of
                Left problem ->
                  return $ Left $ Exit.DetailsCannotGetRegistry problem

                Right (Solver.Env cache manager connection registry) ->
                  return $ Right (Env key scope root cache manager connection registry, outline)



-- VERIFY PROJECT


type Task a = Task.Task Exit.Details a


verifyPkg :: Env -> File.Time -> Outline.PkgOutline -> Task Details
verifyPkg env time (Outline.PkgOutline pkg _ _ _ exposed direct testDirect elm) =
  if Con.goodElm elm
  then
    do  solution <- verifyConstraints env =<< union noDups direct testDirect
        let exposedList = Outline.flattenExposed exposed
        let exactDeps = Map.map (\(Solver.Details v _) -> v) solution -- for pkg docs in reactor
        verifyDependencies env time (ValidPkg pkg exposedList exactDeps) solution direct
  else
    Task.throw $ Exit.DetailsBadElmInPkg elm


verifyApp :: Env -> File.Time -> Outline.AppOutline -> Task Details
verifyApp env time outline@(Outline.AppOutline elmVersion srcDirs direct _ _ _) =
  if elmVersion == V.compiler
  then
    do  stated <- checkAppDeps outline
        actual <- verifyConstraints env (Map.map Con.exactly stated)
        if Map.size stated == Map.size actual
          then verifyDependencies env time (ValidApp srcDirs) actual direct
          else Task.throw $ Exit.DetailsHandEditedDependencies
  else
    Task.throw $ Exit.DetailsBadElmInAppOutline elmVersion


checkAppDeps :: Outline.AppOutline -> Task (Map.Map Pkg.Name V.Version)
checkAppDeps (Outline.AppOutline _ _ direct indirect testDirect testIndirect) =
  do  x <- union allowEqualDups indirect testDirect
      y <- union noDups direct testIndirect
      union noDups x y



-- VERIFY CONSTRAINTS


verifyConstraints :: Env -> Map.Map Pkg.Name Con.Constraint -> Task (Map.Map Pkg.Name Solver.Details)
verifyConstraints (Env _ _ _ cache _ connection registry) constraints =
  do  result <- Task.io $ Solver.verify cache connection registry constraints
      case result of
        Solver.Ok details        -> return details
        Solver.NoSolution        -> Task.throw $ Exit.DetailsNoSolution
        Solver.NoOfflineSolution -> Task.throw $ Exit.DetailsNoOfflineSolution
        Solver.Err exit          -> Task.throw $ Exit.DetailsSolverProblem exit



-- UNION


union :: (Ord k) => (k -> v -> v -> Task v) -> Map.Map k v -> Map.Map k v -> Task (Map.Map k v)
union tieBreaker deps1 deps2 =
  Map.mergeA Map.preserveMissing Map.preserveMissing (Map.zipWithAMatched tieBreaker) deps1 deps2


noDups :: k -> v -> v -> Task v
noDups _ _ _ =
  Task.throw Exit.DetailsHandEditedDependencies


allowEqualDups :: (Eq v) => k -> v -> v -> Task v
allowEqualDups _ v1 v2 =
  if v1 == v2
  then return v1
  else Task.throw Exit.DetailsHandEditedDependencies



-- FORK


fork :: IO a -> IO (MVar a)
fork work =
  do  mvar <- newEmptyMVar
      _ <- forkIO $ putMVar mvar =<< work
      return mvar



-- VERIFY DEPENDENCIES


verifyDependencies :: Env -> File.Time -> ValidOutline -> Map.Map Pkg.Name Solver.Details -> Map.Map Pkg.Name a -> Task Details
verifyDependencies env@(Env key scope root cache _ _ _) time outline solution directDeps =
  Task.eio id $
  do  Reporting.report key (Reporting.DStart (Map.size solution))
      mvar <- newEmptyMVar
      mvars <- Stuff.withRegistryLock cache $
        Map.traverseWithKey (\k v -> fork (verifyDep env mvar solution k v)) solution
      putMVar mvar mvars
      deps <- traverse readMVar mvars
      case sequence deps of
        Left _ ->
          do  home <- Stuff.getElmHome
              return $ Left $ Exit.DetailsBadDeps home $
                Maybe.catMaybes $ Either.lefts $ Map.elems deps

        Right artifacts ->
          let
            objs = Map.foldr addObjects Opt.empty artifacts
            ifaces = Map.foldrWithKey (addInterfaces directDeps) Map.empty artifacts
            foreigns = Map.map (OneOrMore.destruct Foreign) $ Map.foldrWithKey gatherForeigns Map.empty $ Map.intersection artifacts directDeps
            details = Details time outline 0 Map.empty foreigns (ArtifactsFresh ifaces objs)
          in
          do  BW.writeBinary scope (Stuff.objects    root) objs
              BW.writeBinary scope (Stuff.interfaces root) ifaces
              BW.writeBinary scope (Stuff.details    root) details
              return (Right details)


addObjects :: Artifacts -> Opt.GlobalGraph -> Opt.GlobalGraph
addObjects (Artifacts _ objs) graph =
  Opt.addGlobalGraph objs graph


addInterfaces :: Map.Map Pkg.Name a -> Pkg.Name -> Artifacts -> Interfaces -> Interfaces
addInterfaces directDeps pkg (Artifacts ifaces _) dependencyInterfaces =
  Map.union dependencyInterfaces $ Map.mapKeysMonotonic (ModuleName.Canonical pkg) $
    if Map.member pkg directDeps
      then ifaces
      else Map.map I.privatize ifaces


gatherForeigns :: Pkg.Name -> Artifacts -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore Pkg.Name) -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore Pkg.Name)
gatherForeigns pkg (Artifacts ifaces _) foreigns =
  let
    isPublic di =
      case di of
        I.Public _      -> Just (OneOrMore.one pkg)
        I.Private _ _ _ -> Nothing
  in
  Map.unionWith OneOrMore.more foreigns (Map.mapMaybe isPublic ifaces)



-- VERIFY DEPENDENCY


data Artifacts =
  Artifacts
    { _ifaces :: Map.Map ModuleName.Raw I.DependencyInterface
    , _objects :: Opt.GlobalGraph
    }


type Dep =
  Either (Maybe Exit.DetailsBadDep) Artifacts


verifyDep :: Env -> MVar (Map.Map Pkg.Name (MVar Dep)) -> Map.Map Pkg.Name Solver.Details -> Pkg.Name -> Solver.Details -> IO Dep
verifyDep (Env key _ _ cache manager _ _) depsMVar solution pkg details@(Solver.Details vsn directDeps) =
  do  let fingerprint = Map.intersectionWith (\(Solver.Details v _) _ -> v) solution directDeps
      exists <- Dir.doesDirectoryExist (Stuff.package cache pkg vsn </> "src")
      if exists
        then
          do  Reporting.report key Reporting.DCached
              maybeCache <- File.readBinary (Stuff.package cache pkg vsn </> "artifacts.dat")
              case maybeCache of
                Nothing ->
                  build key cache depsMVar pkg details fingerprint Set.empty

                Just (ArtifactCache fingerprints artifacts) ->
                  if Set.member fingerprint fingerprints
                    then Reporting.report key Reporting.DBuilt >> return (Right artifacts)
                    else build key cache depsMVar pkg details fingerprint fingerprints
        else
          do  Reporting.report key Reporting.DRequested
              result <- downloadPackage cache manager pkg vsn
              case result of
                Left problem ->
                  do  Reporting.report key (Reporting.DFailed pkg vsn)
                      return $ Left $ Just $ Exit.BD_BadDownload pkg vsn problem

                Right () ->
                  do  Reporting.report key (Reporting.DReceived pkg vsn)
                      build key cache depsMVar pkg details fingerprint Set.empty



-- ARTIFACT CACHE


data ArtifactCache =
  ArtifactCache
    { _fingerprints :: Set.Set Fingerprint
    , _artifacts :: Artifacts
    }


type Fingerprint =
  Map.Map Pkg.Name V.Version



-- BUILD


build :: Reporting.DKey -> Stuff.PackageCache -> MVar (Map.Map Pkg.Name (MVar Dep)) -> Pkg.Name -> Solver.Details -> Fingerprint -> Set.Set Fingerprint -> IO Dep
build key cache depsMVar pkg (Solver.Details vsn _) f fs =
  do  eitherOutline <- Outline.read (Stuff.package cache pkg vsn)
      case eitherOutline of
        Left _ ->
          do  Reporting.report key Reporting.DBroken
              return $ Left $ Just $ Exit.BD_BadBuild pkg vsn f

        Right (Outline.App _) ->
          do  Reporting.report key Reporting.DBroken
              return $ Left $ Just $ Exit.BD_BadBuild pkg vsn f

        Right (Outline.Pkg (Outline.PkgOutline _ _ _ _ exposed deps _ _)) ->
          do  allDeps <- readMVar depsMVar
              directDeps <- traverse readMVar (Map.intersection allDeps deps)
              case sequence directDeps of
                Left _ ->
                  do  Reporting.report key Reporting.DBroken
                      return $ Left $ Nothing

                Right directArtifacts ->
                  do  let src = Stuff.package cache pkg vsn </> "src"
                      let foreignDeps = gatherForeignInterfaces directArtifacts
                      let exposedDict = Map.fromKeys (\_ -> ()) (Outline.flattenExposed exposed)
                      docsStatus <- getDocsStatus cache pkg vsn
                      mvar <- newEmptyMVar
                      mvars <- Map.traverseWithKey (const . fork . crawlModule foreignDeps mvar pkg src docsStatus) exposedDict
                      putMVar mvar mvars
                      mapM_ readMVar mvars
                      maybeStatuses <- traverse readMVar =<< readMVar mvar
                      case sequence maybeStatuses of
                        Nothing ->
                          do  Reporting.report key Reporting.DBroken
                              return $ Left $ Just $ Exit.BD_BadBuild pkg vsn f

                        Just statuses ->
                          do  rmvar <- newEmptyMVar
                              rmvars <- traverse (fork . compile pkg rmvar) statuses
                              putMVar rmvar rmvars
                              maybeResults <- traverse readMVar rmvars
                              case sequence maybeResults of
                                Nothing ->
                                  do  Reporting.report key Reporting.DBroken
                                      return $ Left $ Just $ Exit.BD_BadBuild pkg vsn f

                                Just results ->
                                  let
                                    path = Stuff.package cache pkg vsn </> "artifacts.dat"
                                    ifaces = gatherInterfaces exposedDict results
                                    objects = gatherObjects results
                                    artifacts = Artifacts ifaces objects
                                    fingerprints = Set.insert f fs
                                  in
                                  do  writeDocs cache pkg vsn docsStatus results
                                      File.writeBinary path (ArtifactCache fingerprints artifacts)
                                      Reporting.report key Reporting.DBuilt
                                      return (Right artifacts)



-- GATHER


gatherObjects :: Map.Map ModuleName.Raw Result -> Opt.GlobalGraph
gatherObjects results =
  Map.foldrWithKey addLocalGraph Opt.empty results


addLocalGraph :: ModuleName.Raw -> Result -> Opt.GlobalGraph -> Opt.GlobalGraph
addLocalGraph name status graph =
  case status of
    RLocal _ objs _ -> Opt.addLocalGraph objs graph
    RForeign _      -> graph
    RKernelLocal cs -> Opt.addKernel (Name.getKernel name) cs graph
    RKernelForeign  -> graph


gatherInterfaces :: Map.Map ModuleName.Raw () -> Map.Map ModuleName.Raw Result -> Map.Map ModuleName.Raw I.DependencyInterface
gatherInterfaces exposed artifacts =
  let
    onLeft  = Map.mapMissing (error "compiler bug manifesting in Elm.Details.gatherInterfaces")
    onRight = Map.mapMaybeMissing     (\_    iface -> toLocalInterface I.private iface)
    onBoth  = Map.zipWithMaybeMatched (\_ () iface -> toLocalInterface I.public  iface)
  in
  Map.merge onLeft onRight onBoth exposed artifacts


toLocalInterface :: (I.Interface -> a) -> Result -> Maybe a
toLocalInterface func result =
  case result of
    RLocal iface _ _ -> Just (func iface)
    RForeign _       -> Nothing
    RKernelLocal _   -> Nothing
    RKernelForeign   -> Nothing



-- GATHER FOREIGN INTERFACES


data ForeignInterface
  = ForeignAmbiguous
  | ForeignSpecific I.Interface


gatherForeignInterfaces :: Map.Map Pkg.Name Artifacts -> Map.Map ModuleName.Raw ForeignInterface
gatherForeignInterfaces directArtifacts =
    Map.map (OneOrMore.destruct finalize) $
      Map.foldrWithKey gather Map.empty directArtifacts
  where
    finalize :: I.Interface -> [I.Interface] -> ForeignInterface
    finalize i is =
      case is of
        [] -> ForeignSpecific i
        _:_ -> ForeignAmbiguous

    gather :: Pkg.Name -> Artifacts -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore I.Interface) -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore I.Interface)
    gather _ (Artifacts ifaces _) buckets =
      Map.unionWith OneOrMore.more buckets (Map.mapMaybe isPublic ifaces)

    isPublic :: I.DependencyInterface -> Maybe (OneOrMore.OneOrMore I.Interface)
    isPublic di =
      case di of
        I.Public iface  -> Just (OneOrMore.one iface)
        I.Private _ _ _ -> Nothing



-- CRAWL


type StatusDict =
  Map.Map ModuleName.Raw (MVar (Maybe Status))


data Status
  = SLocal DocsStatus (Map.Map ModuleName.Raw ()) Src.Module
  | SForeign I.Interface
  | SKernelLocal [Kernel.Chunk]
  | SKernelForeign


crawlModule :: Map.Map ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> DocsStatus -> ModuleName.Raw -> IO (Maybe Status)
crawlModule foreignDeps mvar pkg src docsStatus name =
  do  let path = src </> ModuleName.toFilePath name <.> "elm"
      exists <- File.exists path
      case Map.lookup name foreignDeps of
        Just ForeignAmbiguous ->
          return Nothing

        Just (ForeignSpecific iface) ->
          if exists
          then return Nothing
          else return (Just (SForeign iface))

        Nothing ->
          if exists then
            crawlFile foreignDeps mvar pkg src docsStatus name path

          else if Pkg.isKernel pkg && Name.isKernel name then
            crawlKernel foreignDeps mvar pkg src name

          else
            return Nothing


crawlFile :: Map.Map ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> DocsStatus -> ModuleName.Raw -> FilePath -> IO (Maybe Status)
crawlFile foreignDeps mvar pkg src docsStatus expectedName path =
  do  bytes <- File.readUtf8 path
      case Parse.fromByteString (Parse.Package pkg) bytes of
        Right modul@(Src.Module (Just (A.At _ actualName)) _ _ imports _ _ _ _ _) | expectedName == actualName ->
          do  deps <- crawlImports foreignDeps mvar pkg src imports
              return (Just (SLocal docsStatus deps modul))

        _ ->
          return Nothing


crawlImports :: Map.Map ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> [Src.Import] -> IO (Map.Map ModuleName.Raw ())
crawlImports foreignDeps mvar pkg src imports =
  do  statusDict <- takeMVar mvar
      let deps = Map.fromList (map (\i -> (Src.getImportName i, ())) imports)
      let news = Map.difference deps statusDict
      mvars <- Map.traverseWithKey (const . fork . crawlModule foreignDeps mvar pkg src DocsNotNeeded) news
      putMVar mvar (Map.union mvars statusDict)
      mapM_ readMVar mvars
      return deps


crawlKernel :: Map.Map ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> ModuleName.Raw -> IO (Maybe Status)
crawlKernel foreignDeps mvar pkg src name =
  do  let path = src </> ModuleName.toFilePath name <.> "js"
      exists <- File.exists path
      if exists
        then
          do  bytes <- File.readUtf8 path
              case Kernel.fromByteString pkg (Map.mapMaybe getDepHome foreignDeps) bytes of
                Nothing ->
                  return Nothing

                Just (Kernel.Content imports chunks) ->
                  do  _ <- crawlImports foreignDeps mvar pkg src imports
                      return (Just (SKernelLocal chunks))
        else
          return (Just SKernelForeign)


getDepHome :: ForeignInterface -> Maybe Pkg.Name
getDepHome fi =
  case fi of
    ForeignSpecific (I.Interface pkg _ _ _ _) -> Just pkg
    ForeignAmbiguous                          -> Nothing



-- COMPILE


data Result
  = RLocal !I.Interface !Opt.LocalGraph (Maybe Docs.Module)
  | RForeign I.Interface
  | RKernelLocal [Kernel.Chunk]
  | RKernelForeign


compile :: Pkg.Name -> MVar (Map.Map ModuleName.Raw (MVar (Maybe Result))) -> Status -> IO (Maybe Result)
compile pkg mvar status =
  case status of
    SLocal docsStatus deps modul ->
      do  resultsDict <- readMVar mvar
          maybeResults <- traverse readMVar (Map.intersection resultsDict deps)
          case sequence maybeResults of
            Nothing ->
              return Nothing

            Just results ->
              case Compile.compile pkg (Map.mapMaybe getInterface results) modul of
                Left _ ->
                  return Nothing

                Right (Compile.Artifacts canonical annotations objects) ->
                  let
                    ifaces = I.fromModule pkg canonical annotations
                    docs = makeDocs docsStatus canonical
                  in
                  return (Just (RLocal ifaces objects docs))

    SForeign iface ->
      return (Just (RForeign iface))

    SKernelLocal chunks ->
      return (Just (RKernelLocal chunks))

    SKernelForeign ->
      return (Just RKernelForeign)


getInterface :: Result -> Maybe I.Interface
getInterface result =
  case result of
    RLocal iface _ _ -> Just iface
    RForeign iface   -> Just iface
    RKernelLocal _   -> Nothing
    RKernelForeign   -> Nothing



-- MAKE DOCS


data DocsStatus
  = DocsNeeded
  | DocsNotNeeded


getDocsStatus :: Stuff.PackageCache -> Pkg.Name -> V.Version -> IO DocsStatus
getDocsStatus cache pkg vsn =
  do  exists <- File.exists (Stuff.package cache pkg vsn </> "docs.json")
      if exists
        then return DocsNotNeeded
        else return DocsNeeded


makeDocs :: DocsStatus -> Can.Module -> Maybe Docs.Module
makeDocs status modul =
  case status of
    DocsNeeded ->
      case Docs.fromModule modul of
        Right docs -> Just docs
        Left _     -> Nothing

    DocsNotNeeded ->
      Nothing


writeDocs :: Stuff.PackageCache -> Pkg.Name -> V.Version -> DocsStatus -> Map.Map ModuleName.Raw Result -> IO ()
writeDocs cache pkg vsn status results =
  case status of
    DocsNeeded ->
      E.writeUgly (Stuff.package cache pkg vsn </> "docs.json") $
        Docs.encode $ Map.mapMaybe toDocs results

    DocsNotNeeded ->
      return ()


toDocs :: Result -> Maybe Docs.Module
toDocs result =
  case result of
    RLocal _ _ docs -> docs
    RForeign _      -> Nothing
    RKernelLocal _  -> Nothing
    RKernelForeign  -> Nothing



-- DOWNLOAD PACKAGE


downloadPackage :: Stuff.PackageCache -> Http.Manager -> Pkg.Name -> V.Version -> IO (Either Exit.PackageProblem ())
downloadPackage cache manager pkg vsn =
  let
    url = Website.metadata pkg vsn "endpoint.json"
  in
  do  eitherByteString <-
        Http.get manager url [] id (return . Right)

      case eitherByteString of
        Left err ->
          return $ Left $ Exit.PP_BadEndpointRequest err

        Right byteString ->
          case D.fromByteString endpointDecoder byteString of
            Left _ ->
              return $ Left $ Exit.PP_BadEndpointContent url

            Right (endpoint, expectedHash) ->
              Http.getArchive manager endpoint Exit.PP_BadArchiveRequest (Exit.PP_BadArchiveContent endpoint) $
                \(sha, archive) ->
                  if expectedHash == Http.shaToChars sha
                  then Right <$> File.writePackage (Stuff.package cache pkg vsn) archive
                  else return $ Left $ Exit.PP_BadArchiveHash endpoint expectedHash (Http.shaToChars sha)


endpointDecoder :: D.Decoder e (String, String)
endpointDecoder =
  do  url <- D.field "url" D.string
      hash <- D.field "hash" D.string
      return (Utf8.toChars url, Utf8.toChars hash)



-- BINARY


instance Binary Details where
  put (Details a b c d e _) = put a >> put b >> put c >> put d >> put e
  get =
    do  a <- get
        b <- get
        c <- get
        d <- get
        e <- get
        return (Details a b c d e ArtifactsCached)


instance Binary ValidOutline where
  put outline =
    case outline of
      ValidApp a     -> putWord8 0 >> put a
      ValidPkg a b c -> putWord8 1 >> put a >> put b >> put c

  get =
    do  n <- getWord8
        case n of
          0 -> liftM  ValidApp get
          1 -> liftM3 ValidPkg get get get
          _ -> fail "binary encoding of ValidOutline was corrupted"


instance Binary Local where
  put (Local a b c d e f) = put a >> put b >> put c >> put d >> put e >> put f
  get =
    do  a <- get
        b <- get
        c <- get
        d <- get
        e <- get
        f <- get
        return (Local a b c d e f)


instance Binary Foreign where
  get = liftM2 Foreign get get
  put (Foreign a b) = put a >> put b


instance Binary Artifacts where
  get = liftM2 Artifacts get get
  put (Artifacts a b) = put a >> put b


instance Binary ArtifactCache where
  get = liftM2 ArtifactCache get get
  put (ArtifactCache a b) = put a >> put b
