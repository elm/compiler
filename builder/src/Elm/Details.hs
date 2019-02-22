{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Elm.Details
  ( Details(..)
  , ValidOutline(..)
  , Local(..)
  , Foreign(..)
  , verify
  )
  where


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Monad (liftM, liftM2, liftM3, liftM4)
import Data.Binary (Binary, get, put, getWord8, putWord8)
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.OneOrMore as OneOrMore
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))

import qualified AST.Source as Src
import qualified AST.Optimized as Opt
import qualified Compile
import qualified Deps.Registry as Registry
import qualified Deps.Solver as Solver
import qualified Deps.Website as Website
import qualified Elm.Constraint as Con
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified File
import qualified Http
import qualified Json.Decode as D
import qualified Parse.Module as Parse
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff



-- DETAILS


data Details =
  Details
    { _outlineTime :: File.Time
    , _outline :: ValidOutline
    , _locals :: Map.Map ModuleName.Raw Local
    , _foreigns :: Map.Map ModuleName.Raw Foreign
    }


data ValidOutline
  = ValidApp Outline.AppOutline
  | ValidPkg Outline.PkgOutline (Map.Map Pkg.Name V.Version)


data Local =
  Local
    { _path :: FilePath
    , _time :: File.Time
    , _deps :: [ModuleName.Raw]
    }


data Foreign =
  Foreign Pkg.Name [Pkg.Name]



-- VERIFY


verify :: FilePath -> Task.Task Details
verify root =
  do  newTime      <- Task.io $ File.getTime (root </> "elm.json")
      maybeDetails <- Task.io $ File.readBinary (root </> Stuff.details)
      case maybeDetails of
        Nothing ->
          rebuild root newTime

        Just details@(Details oldTime _ _ _) ->
          if oldTime == newTime
          then return details
          else rebuild root newTime



-- REBUILD


rebuild :: FilePath -> File.Time -> Task.Task Details
rebuild root time =
  do  (outline, info) <- rebuildHelp root
      case outline of
        Outline.Pkg pkg -> solvePkg info time pkg
        Outline.App app -> solveApp info time app


data Info =
  Info Stuff.PackageCache Http.Manager Connection Registry.Registry


data Connection
  = Online
  | Offline


rebuildHelp :: FilePath -> Task.Task (Outline.Outline, Info)
rebuildHelp root =
  Task.eio id $
  do  cacheMVar     <- fork Stuff.getPackageCache
      managerMVar   <- fork Http.getManager
      registryMVar  <- fork (Registry.read =<< readMVar cacheMVar)
      eitherOutline <- Outline.read root
      cache         <- readMVar cacheMVar
      manager       <- readMVar managerMVar
      maybeRegistry <- readMVar registryMVar
      case eitherOutline of
        Left problem ->
          return $ Left $ Exit.DetailsBadOutline problem

        Right outline ->
          case maybeRegistry of
            Nothing ->
              do  eitherRegistry <- Registry.fetch manager cache
                  case eitherRegistry of
                    Right latestRegistry ->
                      return $ Right (outline, Info cache manager Online latestRegistry)

                    Left problem ->
                      return $ Left $ Exit.DetailsCannotGetRegistry problem

            Just cachedRegistry ->
              do  eitherRegistry <- Registry.update manager cache cachedRegistry
                  case eitherRegistry of
                    Right latestRegistry ->
                      return $ Right (outline, Info cache manager Online latestRegistry)

                    Left _ ->
                      return $ Right (outline, Info cache manager Offline cachedRegistry)



-- SOLVE PKG


solvePkg :: Info -> File.Time -> Outline.PkgOutline -> Task.Task Details
solvePkg info time outline@(Outline.PkgOutline _ _ _ _ _ direct testDirect elm) =
  if Con.goodElm elm
  then
    do  solution <- solve info =<< union noDups direct testDirect
        foreigns <- verifyDependencies info solution direct
        let vsns = Map.map (\(Solver.Details v _) -> v) solution
        return $ Details time (ValidPkg outline vsns) Map.empty foreigns
  else
    Task.throw $ Exit.DetailsBadElmInPkg elm



-- SOLVE APP


solveApp :: Info -> File.Time -> Outline.AppOutline -> Task.Task Details
solveApp info time outline@(Outline.AppOutline elmVersion _ direct _ _ _) =
  if elmVersion == V.compiler
  then
    do  stated <- checkAppDeps outline
        actual <- solve info (Map.map Con.exactly stated)
        if Map.size stated == Map.size actual
          then
            do  foreigns <- verifyDependencies info actual direct
                return $ Details time (ValidApp outline) Map.empty foreigns
          else
            Task.throw $ error "TODO solveApp"
  else
    Task.throw $ Exit.DetailsBadElmInApp elmVersion


checkAppDeps :: Outline.AppOutline -> Task.Task (Map.Map Pkg.Name V.Version)
checkAppDeps (Outline.AppOutline _ _ direct indirect testDirect testIndirect) =
  do  x <- union allowEqualDups indirect testDirect
      y <- union noDups direct testIndirect
      union noDups x y



-- SOLVE


solve :: Info -> Map.Map Pkg.Name Con.Constraint -> Task.Task (Map.Map Pkg.Name Solver.Details)
solve (Info cache manager connection registry) constraints =
  Task.eio (error "TODO solve problem") $
    case connection of
      Online -> Solver.online cache manager registry constraints
      Offline -> Solver.offline cache registry constraints



-- UNION


union :: (Ord k) => (k -> v -> v -> Task.Task v) -> Map.Map k v -> Map.Map k v -> Task.Task (Map.Map k v)
union tieBreaker deps1 deps2 =
  Map.mergeA Map.preserveMissing Map.preserveMissing (Map.zipWithAMatched tieBreaker) deps1 deps2


noDups :: k -> v -> v -> Task.Task v
noDups _ _ _ =
  Task.throw Exit.DetailsHandEditedDependencies


allowEqualDups :: (Eq v) => k -> v -> v -> Task.Task v
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


verifyDependencies :: Info -> Map.Map Pkg.Name Solver.Details -> Map.Map Pkg.Name a -> Task.Task (Map.Map ModuleName.Raw Foreign)
verifyDependencies info solution directDeps =
  Task.eio (error "TODO verifyDependencies") $
  do  mvar <- newEmptyMVar
      mvars <- Map.traverseWithKey (\k v -> fork (verifyDep info mvar solution k v)) solution
      putMVar mvar mvars
      results <- traverse readMVar mvars
      case sequence results of
        Left _ ->
          return (Left (error "TODO one of the dependencies did not build"))

        Right artifacts ->
          let
            objs = Map.foldr gatherObjects Opt.empty artifacts
            ifaces = Map.foldrWithKey (gatherInterfaces directDeps) Map.empty artifacts
            foreigns = Map.map (OneOrMore.destruct Foreign) $ Map.foldrWithKey gatherForeigns Map.empty artifacts
          in
          do  File.writeBinary Stuff.objects objs
              File.writeBinary Stuff.interfaces ifaces
              return (Right foreigns)


gatherObjects :: Artifacts -> Opt.Graph -> Opt.Graph
gatherObjects (Artifacts _ objs) graph =
  Opt.union objs graph


gatherInterfaces :: Map.Map Pkg.Name a -> Pkg.Name -> Artifacts -> Map.Map ModuleName.Canonical I.DependencyInterface -> Map.Map ModuleName.Canonical I.DependencyInterface
gatherInterfaces directDeps pkg (Artifacts ifaces _) dependencyInterfaces =
  Map.union dependencyInterfaces $ Map.mapKeys (ModuleName.Canonical pkg) $
    if Map.member pkg directDeps
      then ifaces
      else Map.map I.privatize ifaces


gatherForeigns :: Pkg.Name -> Artifacts -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore Pkg.Name) -> Map.Map ModuleName.Raw (OneOrMore.OneOrMore Pkg.Name)
gatherForeigns pkg (Artifacts ifaces _) foreigns =
  let
    isPublic di =
      case di of
        I.Public _    -> Just (OneOrMore.one pkg)
        I.Private _ _ -> Nothing
  in
  Map.unionWith OneOrMore.more foreigns (Map.mapMaybe isPublic ifaces)



-- VERIFY DEPENDENCY


data Artifacts =
  Artifacts
    { _ifaces :: Map.Map ModuleName.Raw I.DependencyInterface
    , _objects :: Opt.Graph
    }


type Result =
  Either Problem Artifacts


data Problem
  = BadDownload Exit.PackageProblem
  | BadBuild
  | BadDependency


verifyDep :: Info -> MVar (Map.Map Pkg.Name (MVar Result)) -> Map.Map Pkg.Name Solver.Details -> Pkg.Name -> Solver.Details -> IO Result
verifyDep (Info cache manager _ _) resultsMVar solution name details@(Solver.Details version directDeps) =
  let
    fingerprint = Map.intersectionWith (\(Solver.Details v _) _ -> v) solution directDeps
  in
  do  exists <- Dir.doesDirectoryExist (Stuff.package cache name version </> "src")
      if exists
        then
          do  maybeCache <- File.readBinary (Stuff.package cache name version </> "artifacts.dat")
              case maybeCache of
                Nothing ->
                  build cache resultsMVar name details (Set.singleton fingerprint)

                Just (ArtifactCache fingerprints artifacts) ->
                  if Set.member fingerprint fingerprints
                    then return (Right artifacts)
                    else build cache resultsMVar name details (Set.insert fingerprint fingerprints)
        else
          do  result <- downloadPackage cache manager name version
              case result of
                Left p -> return (Left (BadDownload p))
                Right () -> build cache resultsMVar name details (Set.singleton fingerprint)



-- ARTIFACT CACHE


data ArtifactCache =
  ArtifactCache
    { _fingerprints :: Set.Set Fingerprint
    , _artifacts :: Artifacts
    }


type Fingerprint =
  Map.Map Pkg.Name V.Version



-- BUILD


build :: Stuff.PackageCache -> MVar (Map.Map Pkg.Name (MVar Result)) -> Pkg.Name -> Solver.Details -> Set.Set Fingerprint -> IO Result
build cache resultsMVar name (Solver.Details version _) fingerprints =
  do  eitherOutline <- Outline.read (Stuff.package cache name version </> "elm.json")
      case eitherOutline of
        Left _ ->
          return (Left BadBuild)

        Right (Outline.App _) ->
          return (Left BadBuild)

        Right (Outline.Pkg (Outline.PkgOutline _ _ _ _ exposed deps _ _)) ->
          do  allResults <- readMVar resultsMVar
              directResults <- traverse readMVar (Map.intersection allResults deps)
              case sequence directResults of
                Left _ ->
                  return (Left BadDependency)

                Right directArtifacts ->
                  do  mvar <- newMVar Map.empty
                      let src = Stuff.package cache name version </> "src"
                      let foreignDeps = gatherForeignInterfaces directArtifacts
                      let exposedDict = Map.fromList $ map (\n -> (n,())) (Outline.flattenExposed exposed)
                      traverse_ readMVar =<<
                        Map.traverseWithKey (const . fork . buildModule foreignDeps mvar name src) exposedDict
                      result <- traverse readMVar =<< readMVar mvar
                      case Map.traverseMaybeWithKey checkStatus result of
                        Nothing ->
                          return (Left BadBuild)

                        Just artifactsDict ->
                          let
                            path = Stuff.package cache name version </> "artifacts.dat"
                            ifaces = gatherPackageInterfaces exposedDict artifactsDict
                            objects = Map.foldr (Opt.union . snd) Opt.empty artifactsDict
                            artifacts = Artifacts ifaces objects
                          in
                          do  File.writeBinary path (ArtifactCache fingerprints artifacts)
                              return (Right artifacts)


checkStatus :: ModuleName.Raw -> Status -> Maybe (Maybe (I.Interface, Opt.Graph))
checkStatus _ status =
  case status of
    SFailure   -> Nothing
    SLocal i g -> Just (Just (i,g))
    SForeign _ -> Just Nothing


gatherPackageInterfaces :: Map.Map ModuleName.Raw () -> Map.Map ModuleName.Raw (I.Interface, Opt.Graph) -> Map.Map ModuleName.Raw I.DependencyInterface
gatherPackageInterfaces exposed artifacts =
    Map.merge onLeft onRight onBoth exposed artifacts
  where
    onLeft = Map.mapMissing (error "compiler bug manifesting in Elm.Details.gatherPackageInterfaces")
    onRight = Map.mapMissing (\_ (i,_) -> I.private i)
    onBoth = Map.zipWithMatched (\_ () (i,_) -> I.public i)



-- FOREIGN INTERFACES


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
        I.Public iface -> Just (OneOrMore.one iface)
        I.Private _ _ -> Nothing



-- BUILD MODULE


type StatusDict =
  Map.Map ModuleName.Raw (MVar Status)


data Status
  = SLocal !I.Interface !Opt.Graph
  | SForeign I.Interface
  | SFailure


buildModule :: Map.Map ModuleName.Raw ForeignInterface -> MVar StatusDict -> Pkg.Name -> FilePath -> ModuleName.Raw -> IO Status
buildModule foreignDeps mvar pkg src name =
  do  let path = src </> ModuleName.toFilePath name <.> "elm"
      exists <- File.exists path
      case Map.lookup name foreignDeps of
        Just ForeignAmbiguous ->
          return SFailure

        Just (ForeignSpecific iface) ->
          if exists
          then return SFailure
          else return (SForeign iface)

        Nothing ->
          if not exists
          then return SFailure
          else
            do  result <- Parse.fromFile pkg path
                case result of
                  Right modul@(Src.Module (Just actualName) _ deps _ _ _ _ _) | name == actualName ->
                    do  statusDict <- takeMVar mvar
                        let depsDict = Map.fromList $ map (\i -> (Src.getImport i, ())) deps
                        let newsDict = Map.difference depsDict statusDict
                        statusMVars <- Map.traverseWithKey (const . fork . buildModule foreignDeps mvar pkg src) newsDict
                        putMVar mvar (Map.union statusMVars statusDict)
                        statuses <- traverse readMVar statusMVars
                        case traverse getInterface statuses of
                          Nothing ->
                            return SFailure

                          Just imports ->
                            case Compile.compile pkg imports modul of
                              (_, Right (Compile.Artifacts canonical annotations objects)) ->
                                return (SLocal (I.fromModule pkg canonical annotations) objects)

                              (_, Left _) ->
                                return SFailure

                  _ ->
                    return SFailure


getInterface :: Status -> Maybe I.Interface
getInterface status =
  case status of
    SLocal iface _ -> Just iface
    SForeign iface -> Just iface
    SFailure       -> Nothing



-- DOWNLOAD PACKAGE


downloadPackage :: Stuff.PackageCache -> Http.Manager -> Pkg.Name -> V.Version -> IO (Either Exit.PackageProblem ())
downloadPackage cache manager name version =
  do  eitherByteString <-
        Http.post manager (Website.metadata name version "endpoint.json") [] id (return . Right)

      case eitherByteString of
        Left err ->
          return (Left (Exit.PP_BadEndpointRequest err))

        Right byteString ->
          case D.fromByteString endpointDecoder byteString of
            Left _ ->
              return (Left Exit.PP_BadEndpointContent)

            Right (endpoint, expectedHash) ->
              Http.fetchArchive manager endpoint Exit.PP_BadArchiveRequest Exit.PP_BadArchiveContent $
                \(sha, archive) ->
                  if expectedHash == Http.shaToChars sha
                  then Right <$> File.writeArchive (Stuff.package cache name version) archive
                  else return $ Left $ Exit.PP_BadArchiveHash expectedHash (Http.shaToChars sha)


endpointDecoder :: D.Decoder e (String, String)
endpointDecoder =
  do  url <- D.field "url" D.string
      hash <- D.field "hash" D.string
      return (Utf8.toChars url, Utf8.toChars hash)



-- BINARY


instance Binary Details where
  get = liftM4 Details get get get get
  put (Details a b c d) = put a >> put b >> put c >> put d


instance Binary ValidOutline where
  put outline =
    case outline of
      ValidApp a   -> putWord8 0 >> put a
      ValidPkg a b -> putWord8 1 >> put a >> put b

  get =
    do  n <- getWord8
        case n of
          0 -> liftM  ValidApp get
          1 -> liftM2 ValidPkg get get
          _ -> error "binary encoding of ValidOutline was corrupted"


instance Binary Local where
  get = liftM3 Local get get get
  put (Local a b c) = put a >> put b >> put c


instance Binary Foreign where
  get = liftM2 Foreign get get
  put (Foreign a b) = put a >> put b


instance Binary Artifacts where
  get = liftM2 Artifacts get get
  put (Artifacts a b) = put a >> put b


instance Binary ArtifactCache where
  get = liftM2 ArtifactCache get get
  put (ArtifactCache a b) = put a >> put b
