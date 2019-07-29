{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE BangPatterns, GADTs, OverloadedStrings #-}
module Build
  ( fromExposed
  , fromMains
  , fromRepl
  , Artifacts(..)
  , Main(..)
  , Module(..)
  , CachedInterface(..)
  , ReplArtifacts(..)
  , DocsGoal(..)
  , getMainNames
  )
  where


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (filterM, mapM_, sequence_)
import qualified Data.ByteString as B
import qualified Data.Char as Char
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map.Utils as Map
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE
import qualified Data.OneOrMore as OneOrMore
import qualified Data.Set as Set
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import System.FilePath ((</>), (<.>))

import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified AST.Optimized as Opt
import qualified Compile
import qualified Elm.Details as Details
import qualified Elm.Docs as Docs
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified File
import qualified Json.Encode as E
import qualified Parse.Module as Parse
import qualified Reporting
import qualified Reporting.Annotation as A
import qualified Reporting.Error as Error
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Error.Import as Import
import qualified Reporting.Exit as Exit
import qualified Reporting.Render.Type.Localizer as L
import qualified Stuff



-- ENVIRONMENT


data Env =
  Env
    { _key :: Reporting.BKey
    , _root :: FilePath
    , _project :: Parse.ProjectType
    , _srcDirs :: [FilePath]
    , _buildID :: Details.BuildID
    , _locals :: Map.Map ModuleName.Raw Details.Local
    , _foreigns :: Map.Map ModuleName.Raw Details.Foreign
    }


makeEnv :: Reporting.BKey -> FilePath -> Details.Details -> Env
makeEnv key root (Details.Details _ validOutline buildID locals foreigns _) =
  case validOutline of
    Details.ValidApp srcDirs ->
      Env key root Parse.Application (NE.toList srcDirs) buildID locals foreigns

    Details.ValidPkg pkg _ _ ->
      Env key root (Parse.Package pkg) ["src"] buildID locals foreigns



-- FORK


-- PERF try using IORef semephore on file crawl phase?
-- described in Chapter 13 of Parallel and Concurrent Programming in Haskell by Simon Marlow
-- https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch13.html#sec_conc-par-overhead
--
fork :: IO a -> IO (MVar a)
fork work =
  do  mvar <- newEmptyMVar
      _ <- forkIO $ putMVar mvar =<< work
      return mvar


{-# INLINE forkWithKey #-}
forkWithKey :: (k -> a -> IO b) -> Map.Map k a -> IO (Map.Map k (MVar b))
forkWithKey func dict =
  Map.traverseWithKey (\k v -> fork (func k v)) dict



-- FROM EXPOSED


fromExposed :: Reporting.Style -> FilePath -> Details.Details -> DocsGoal docs -> NE.List ModuleName.Raw -> IO (Either Exit.BuildProblem docs)
fromExposed style root details docsGoal exposed@(NE.List e es) =
  Reporting.trackBuild style $ \key ->
  do  let env = makeEnv key root details
      dmvar <- Details.loadInterfaces root details

      -- crawl
      mvar <- newEmptyMVar
      let docsNeed = toDocsNeed docsGoal
      roots <- Map.fromKeysA (fork . crawlModule env mvar docsNeed) (e:es)
      putMVar mvar roots
      mapM_ readMVar roots
      statuses <- traverse readMVar =<< readMVar mvar

      -- compile
      midpoint <- checkMidpoint dmvar statuses
      case midpoint of
        Left problem ->
          return (Left (Exit.BuildProjectProblem problem))

        Right foreigns ->
          do  rmvar <- newEmptyMVar
              resultMVars <- forkWithKey (checkModule env foreigns rmvar) statuses
              putMVar rmvar resultMVars
              results <- traverse readMVar resultMVars
              writeDetails root details results
              finalizeExposed docsGoal exposed results



-- FROM MAINS


data Artifacts =
  Artifacts
    { _name :: Pkg.Name
    , _deps :: Dependencies
    , _mains :: NE.List Main
    , _modules :: [Module]
    }


data Module
  = Fresh ModuleName.Raw I.Interface Opt.LocalGraph
  | Cached ModuleName.Raw Bool (MVar CachedInterface)


type Dependencies =
  Map.Map ModuleName.Canonical I.DependencyInterface


fromMains :: Reporting.Style -> FilePath -> Details.Details -> NE.List FilePath -> IO (Either Exit.BuildProblem Artifacts)
fromMains style root details paths =
  Reporting.trackBuild style $ \key ->
  do  let env = makeEnv key root details

      elmains <- findMains env paths
      case elmains of
        Left problem ->
          return (Left (Exit.BuildProjectProblem problem))

        Right lmains ->
          do  -- crawl
              dmvar <- Details.loadInterfaces root details
              smvar <- newMVar Map.empty
              smainMVars <- traverse (fork . crawlMain env smvar) lmains
              smains <- traverse readMVar smainMVars
              statuses <- traverse readMVar =<< readMVar smvar

              midpoint <- checkMainsMidpoint dmvar statuses smains
              case midpoint of
                Left problem ->
                  return (Left (Exit.BuildProjectProblem problem))

                Right foreigns ->
                  do  -- compile
                      rmvar <- newEmptyMVar
                      resultsMVars <- forkWithKey (checkModule env foreigns rmvar) statuses
                      putMVar rmvar resultsMVars
                      rmainMVars <- traverse (fork . checkMain env resultsMVars) smains
                      results <- traverse readMVar resultsMVars
                      writeDetails root details results
                      toArtifacts env foreigns results <$> traverse readMVar rmainMVars



-- GET MAIN NAMES


getMainNames :: Artifacts -> NE.List ModuleName.Raw
getMainNames (Artifacts _ _ mains _) =
  fmap getMainName mains


getMainName :: Main -> ModuleName.Raw
getMainName main =
  case main of
    Inside  name     -> name
    Outside name _ _ -> name



-- CRAWL


type StatusDict =
  Map.Map ModuleName.Raw (MVar Status)


data Status
  = SCached Details.Local
  | SChanged Details.Local B.ByteString Src.Module DocsNeed
  | SBadImport Import.Problem
  | SBadSyntax FilePath File.Time B.ByteString Syntax.Error
  | SForeign Pkg.Name
  | SKernel


crawlDeps :: Env -> MVar StatusDict -> [ModuleName.Raw] -> a -> IO a
crawlDeps env mvar deps blockedValue =
  do  statusDict <- takeMVar mvar
      let depsDict = Map.fromKeys (\_ -> ()) deps
      let newsDict = Map.difference depsDict statusDict
      statuses <- Map.traverseWithKey crawlNew newsDict
      putMVar mvar (Map.union statuses statusDict)
      mapM_ readMVar statuses
      return blockedValue
  where
    crawlNew name () = fork (crawlModule env mvar (DocsNeed False) name)


crawlModule :: Env -> MVar StatusDict -> DocsNeed -> ModuleName.Raw -> IO Status
crawlModule env@(Env _ root projectType srcDirs buildID locals foreigns) mvar docsNeed name =
  do  let fileName = ModuleName.toFilePath name <.> "elm"
      let inRoot path = File.exists (root </> path)

      paths <- filterM inRoot (map (</> fileName) srcDirs)

      case paths of
        [path] ->
          case Map.lookup name foreigns of
            Just (Details.Foreign dep deps) ->
              return $ SBadImport $ Import.Ambiguous path [] dep deps

            Nothing ->
              do  newTime <- File.getTime path
                  case Map.lookup name locals of
                    Nothing ->
                      crawlFile env mvar docsNeed name path newTime buildID

                    Just local@(Details.Local oldPath oldTime deps _ lastChange _) ->
                      if path /= oldPath || oldTime /= newTime || needsDocs docsNeed
                      then crawlFile env mvar docsNeed name path newTime lastChange
                      else crawlDeps env mvar deps (SCached local)

        p1:p2:ps ->
          return $ SBadImport $ Import.AmbiguousLocal p1 p2 ps

        [] ->
          case Map.lookup name foreigns of
            Just (Details.Foreign dep deps) ->
              case deps of
                [] ->
                  return $ SForeign dep

                d:ds ->
                  return $ SBadImport $ Import.AmbiguousForeign dep d ds

            Nothing ->
              if Name.isKernel name && Parse.isKernel projectType then
                do  exists <- File.exists ("src" </> ModuleName.toFilePath name <.> "js")
                    return $ if exists then SKernel else SBadImport Import.NotFound
              else
                return $ SBadImport Import.NotFound


crawlFile :: Env -> MVar StatusDict -> DocsNeed -> ModuleName.Raw -> FilePath -> File.Time -> Details.BuildID -> IO Status
crawlFile env@(Env _ root projectType _ buildID _ _) mvar docsNeed expectedName path time lastChange =
  do  source <- File.readUtf8 (root </> path)

      case Parse.fromByteString projectType source of
        Left err ->
          return $ SBadSyntax path time source err

        Right modul@(Src.Module maybeActualName _ _ imports values _ _ _ _) ->
          case maybeActualName of
            Nothing ->
              return $ SBadSyntax path time source (Syntax.ModuleNameUnspecified expectedName)

            Just name@(A.At _ actualName) ->
              if expectedName == actualName then
                let
                  deps = map Src.getImportName imports
                  local = Details.Local path time deps (any isMain values) lastChange buildID
                in
                crawlDeps env mvar deps (SChanged local source modul docsNeed)
              else
                return $ SBadSyntax path time source (Syntax.ModuleNameMismatch expectedName name)


isMain :: A.Located Src.Value -> Bool
isMain (A.At _ (Src.Value (A.At _ name) _ _ _)) =
  name == Name._main



-- CHECK MODULE


type ResultDict =
  Map.Map ModuleName.Raw (MVar Result)


data Result
  = RNew !Details.Local !I.Interface !Opt.LocalGraph !(Maybe Docs.Module)
  | RSame !Details.Local !I.Interface !Opt.LocalGraph !(Maybe Docs.Module)
  | RCached Bool Details.BuildID (MVar CachedInterface)
  | RNotFound Import.Problem
  | RProblem Error.Module
  | RBlocked
  | RForeign I.Interface
  | RKernel


data CachedInterface
  = Unneeded
  | Loaded I.Interface
  | Corrupted


checkModule :: Env -> Dependencies -> MVar ResultDict -> ModuleName.Raw -> Status -> IO Result
checkModule env@(Env _ root projectType _ _ _ _) foreigns resultsMVar name status =
  case status of
    SCached local@(Details.Local path time deps hasMain lastChange lastCompile) ->
      do  results <- readMVar resultsMVar
          depsStatus <- checkDeps root results deps lastCompile
          case depsStatus of
            DepsChange ifaces ->
              do  source <- File.readUtf8 path
                  case Parse.fromByteString projectType source of
                    Right modul -> compile env (DocsNeed False) local source ifaces modul
                    Left err ->
                      return $ RProblem $
                        Error.Module name path time source (Error.BadSyntax err)

            DepsSame _ _ ->
              do  mvar <- newMVar Unneeded
                  return (RCached hasMain lastChange mvar)

            DepsBlock ->
              return RBlocked

            DepsNotFound problems ->
              do  source <- File.readUtf8 path
                  return $ RProblem $ Error.Module name path time source $
                    case Parse.fromByteString projectType source of
                      Right (Src.Module _ _ _ imports _ _ _ _ _) ->
                         Error.BadImports (toImportErrors env results imports problems)

                      Left err ->
                        Error.BadSyntax err

    SChanged local@(Details.Local path time deps _ _ lastCompile) source modul@(Src.Module _ _ _ imports _ _ _ _ _) docsNeed ->
      do  results <- readMVar resultsMVar
          depsStatus <- checkDeps root results deps lastCompile
          case depsStatus of
            DepsChange ifaces ->
              compile env docsNeed local source ifaces modul

            DepsSame same cached ->
              do  maybeLoaded <- loadInterfaces root same cached
                  case maybeLoaded of
                    Nothing     -> return RBlocked
                    Just ifaces -> compile env docsNeed local source ifaces modul

            DepsBlock ->
              return RBlocked

            DepsNotFound problems ->
              return $ RProblem $ Error.Module name path time source $
                Error.BadImports (toImportErrors env results imports problems)

    SBadImport importProblem ->
      return (RNotFound importProblem)

    SBadSyntax path time source err ->
      return $ RProblem $ Error.Module name path time source $
        Error.BadSyntax err

    SForeign home ->
      case foreigns ! ModuleName.Canonical home name of
        I.Public iface -> return (RForeign iface)
        I.Private _ _ _ -> error $ "mistakenly seeing private interface for " ++ Pkg.toChars home ++ " " ++ ModuleName.toChars name

    SKernel ->
      return RKernel



-- CHECK DEPS


data DepsStatus
  = DepsChange (Map.Map ModuleName.Raw I.Interface)
  | DepsSame [Dep] [CDep]
  | DepsBlock
  | DepsNotFound (NE.List (ModuleName.Raw, Import.Problem))


checkDeps :: FilePath -> ResultDict -> [ModuleName.Raw] -> Details.BuildID -> IO DepsStatus
checkDeps root results deps lastCompile =
  checkDepsHelp root results deps [] [] [] [] False 0 lastCompile


type Dep = (ModuleName.Raw, I.Interface)
type CDep = (ModuleName.Raw, MVar CachedInterface)


checkDepsHelp :: FilePath -> ResultDict -> [ModuleName.Raw] -> [Dep] -> [Dep] -> [CDep] -> [(ModuleName.Raw,Import.Problem)] -> Bool -> Details.BuildID -> Details.BuildID -> IO DepsStatus
checkDepsHelp root results deps new same cached importProblems isBlocked lastDepChange lastCompile =
  case deps of
    dep:otherDeps ->
      do  result <- readMVar (results ! dep)
          case result of
            RNew (Details.Local _ _ _ _ lastChange _) iface _ _ ->
              checkDepsHelp root results otherDeps ((dep,iface) : new) same cached importProblems isBlocked (max lastChange lastDepChange) lastCompile

            RSame (Details.Local _ _ _ _ lastChange _) iface _ _ ->
              checkDepsHelp root results otherDeps new ((dep,iface) : same) cached importProblems isBlocked (max lastChange lastDepChange) lastCompile

            RCached _ lastChange mvar ->
              checkDepsHelp root results otherDeps new same ((dep,mvar) : cached) importProblems isBlocked (max lastChange lastDepChange) lastCompile

            RNotFound prob ->
              checkDepsHelp root results otherDeps new same cached ((dep,prob) : importProblems) True lastDepChange lastCompile

            RProblem _ ->
              checkDepsHelp root results otherDeps new same cached importProblems True lastDepChange lastCompile

            RBlocked ->
              checkDepsHelp root results otherDeps new same cached importProblems True lastDepChange lastCompile

            RForeign iface ->
              checkDepsHelp root results otherDeps new ((dep,iface) : same) cached importProblems isBlocked lastDepChange lastCompile

            RKernel ->
              checkDepsHelp root results otherDeps new same cached importProblems isBlocked lastDepChange lastCompile


    [] ->
      case reverse importProblems of
        p:ps ->
          return $ DepsNotFound (NE.List p ps)

        [] ->
          if isBlocked then
            return $ DepsBlock

          else if null new && lastDepChange <= lastCompile then
            return $ DepsSame same cached

          else
            do  maybeLoaded <- loadInterfaces root same cached
                case maybeLoaded of
                  Nothing     -> return DepsBlock
                  Just ifaces -> return $ DepsChange $ Map.union (Map.fromList new) ifaces



-- TO IMPORT ERROR


toImportErrors :: Env -> ResultDict -> [Src.Import] -> NE.List (ModuleName.Raw, Import.Problem) -> NE.List Import.Error
toImportErrors (Env _ _ _ _ _ locals foreigns) results imports problems =
  let
    knownModules =
      Set.unions
        [ Map.keysSet foreigns
        , Map.keysSet locals
        , Map.keysSet results
        ]

    unimportedModules =
      Set.difference knownModules (Set.fromList (map Src.getImportName imports))

    regionDict =
      Map.fromList (map (\(Src.Import (A.At region name) _ _) -> (name, region)) imports)

    toError (name, problem) =
      Import.Error (regionDict ! name) name unimportedModules problem
  in
  fmap toError problems



-- LOAD CACHED INTERFACES


loadInterfaces :: FilePath -> [Dep] -> [CDep] -> IO (Maybe (Map.Map ModuleName.Raw I.Interface))
loadInterfaces root same cached =
  do  loading <- traverse (fork . loadInterface root) cached
      maybeLoaded <- traverse readMVar loading
      case sequence maybeLoaded of
        Nothing ->
          return Nothing

        Just loaded ->
          return $ Just $ Map.union (Map.fromList loaded) (Map.fromList same)


loadInterface :: FilePath -> CDep -> IO (Maybe Dep)
loadInterface root (name, ciMvar) =
  do  cachedInterface <- takeMVar ciMvar
      case cachedInterface of
        Corrupted ->
          do  putMVar ciMvar cachedInterface
              return Nothing

        Loaded iface ->
          do  putMVar ciMvar cachedInterface
              return (Just (name, iface))

        Unneeded ->
          do  maybeIface <- File.readBinary (Stuff.elmi root name)
              case maybeIface of
                Nothing ->
                  do  putMVar ciMvar Corrupted
                      return Nothing

                Just iface ->
                  do  putMVar ciMvar (Loaded iface)
                      return (Just (name, iface))



-- CHECK PROJECT


checkMidpoint :: MVar (Maybe Dependencies) -> Map.Map ModuleName.Raw Status -> IO (Either Exit.BuildProjectProblem Dependencies)
checkMidpoint dmvar statuses =
  case checkForCycles statuses of
    Nothing ->
      do  maybeForeigns <- readMVar dmvar
          case maybeForeigns of
            Nothing -> return (Left Exit.BP_CannotLoadDependencies)
            Just fs -> return (Right fs)

    Just (NE.List name names) ->
      do  _ <- readMVar dmvar
          return (Left (Exit.BP_Cycle name names))


checkMainsMidpoint :: MVar (Maybe Dependencies) -> Map.Map ModuleName.Raw Status -> NE.List MainStatus -> IO (Either Exit.BuildProjectProblem Dependencies)
checkMainsMidpoint dmvar statuses smains =
  case checkForCycles statuses of
    Nothing ->
      case checkUniqueMains statuses smains of
        Nothing ->
          do  maybeForeigns <- readMVar dmvar
              case maybeForeigns of
                Nothing -> return (Left Exit.BP_CannotLoadDependencies)
                Just fs -> return (Right fs)

        Just problem ->
          do  _ <- readMVar dmvar
              return (Left problem)

    Just (NE.List name names) ->
      do  _ <- readMVar dmvar
          return (Left (Exit.BP_Cycle name names))



-- CHECK FOR CYCLES


checkForCycles :: Map.Map ModuleName.Raw Status -> Maybe (NE.List ModuleName.Raw)
checkForCycles modules =
  let
    !graph = Map.foldrWithKey addToGraph [] modules
    !sccs = Graph.stronglyConnComp graph
  in
  checkForCyclesHelp sccs


checkForCyclesHelp :: [Graph.SCC ModuleName.Raw] -> Maybe (NE.List ModuleName.Raw)
checkForCyclesHelp sccs =
  case sccs of
    [] ->
      Nothing

    scc:otherSccs ->
      case scc of
        Graph.AcyclicSCC _     -> checkForCyclesHelp otherSccs
        Graph.CyclicSCC []     -> checkForCyclesHelp otherSccs
        Graph.CyclicSCC (m:ms) -> Just (NE.List m ms)


type Node =
  ( ModuleName.Raw, ModuleName.Raw, [ModuleName.Raw] )


addToGraph :: ModuleName.Raw -> Status -> [Node] -> [Node]
addToGraph name status graph =
  let
    dependencies =
      case status of
        SCached  (Details.Local _ _ deps _ _ _)       -> deps
        SChanged (Details.Local _ _ deps _ _ _) _ _ _ -> deps
        SBadImport _                                  -> []
        SBadSyntax _ _ _ _                            -> []
        SForeign _                                    -> []
        SKernel                                       -> []
  in
  (name, name, dependencies) : graph



-- CHECK UNIQUE MAINS


checkUniqueMains :: Map.Map ModuleName.Raw Status -> NE.List MainStatus -> Maybe Exit.BuildProjectProblem
checkUniqueMains insides smains =
  let
    outsidesDict =
      Map.fromListWith OneOrMore.more (Maybe.mapMaybe mainStatusToNamePathPair (NE.toList smains))
  in
  case Map.traverseWithKey checkOutside outsidesDict of
    Left problem ->
      Just problem

    Right outsides ->
      case sequence_ (Map.intersectionWithKey checkInside outsides insides) of
        Right ()     -> Nothing
        Left problem -> Just problem


mainStatusToNamePathPair :: MainStatus -> Maybe (ModuleName.Raw, OneOrMore.OneOrMore FilePath)
mainStatusToNamePathPair smain =
  case smain of
    SInside _                                         -> Nothing
    SOutsideOk (Details.Local path _ _ _ _ _) _ modul -> Just (Src.getName modul, OneOrMore.one path)
    SOutsideErr _                                     -> Nothing


checkOutside :: ModuleName.Raw -> OneOrMore.OneOrMore FilePath -> Either Exit.BuildProjectProblem FilePath
checkOutside name paths =
  case OneOrMore.destruct NE.List paths of
    NE.List p  []     -> Right p
    NE.List p1 (p2:_) -> Left (Exit.BP_MainNameDuplicate name p1 p2)


checkInside :: ModuleName.Raw -> FilePath -> Status -> Either Exit.BuildProjectProblem ()
checkInside name p1 status =
  case status of
    SCached  (Details.Local p2 _ _ _ _ _)       -> Left (Exit.BP_MainNameDuplicate name p1 p2)
    SChanged (Details.Local p2 _ _ _ _ _) _ _ _ -> Left (Exit.BP_MainNameDuplicate name p1 p2)
    SBadImport _                                -> Right ()
    SBadSyntax _ _ _ _                          -> Right ()
    SForeign _                                  -> Right ()
    SKernel                                     -> Right ()



-- COMPILE MODULE


compile :: Env -> DocsNeed -> Details.Local -> B.ByteString -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> IO Result
compile (Env key root projectType _ buildID _ _) docsNeed (Details.Local path time deps main lastChange _) source ifaces modul =
  let
    pkg = projectTypeToPkg projectType
  in
  case Compile.compile pkg ifaces modul of
    Right (Compile.Artifacts canonical annotations objects) ->
      do  let name = Src.getName modul
          let iface = I.fromModule pkg canonical annotations
          let docs = makeDocs docsNeed canonical
          let elmi = Stuff.elmi root name
          File.writeBinary (Stuff.elmo root name) objects
          maybeOldi <- File.readBinary elmi
          case maybeOldi of
            Just oldi | oldi == iface ->
              do  -- iface should be fully forced by equality check
                  Reporting.report key Reporting.BDone
                  let local = Details.Local path time deps main lastChange buildID
                  return (RSame local iface objects docs)

            _ ->
              do  -- iface may be lazy still
                  File.writeBinary elmi iface
                  Reporting.report key Reporting.BDone
                  let local = Details.Local path time deps main buildID buildID
                  return (RNew local iface objects docs)

    Left err ->
      return $ RProblem $
        Error.Module (Src.getName modul) path time source err


projectTypeToPkg :: Parse.ProjectType -> Pkg.Name
projectTypeToPkg projectType =
  case projectType of
    Parse.Package pkg -> pkg
    Parse.Application -> Pkg.dummyName



-- WRITE DETAILS


writeDetails :: FilePath -> Details.Details -> Map.Map ModuleName.Raw Result -> IO ()
writeDetails root (Details.Details time outline buildID locals foreigns extras) results =
  File.writeBinary (Stuff.details root) $
    Details.Details time outline buildID (Map.foldrWithKey addNewLocal locals results) foreigns extras


addNewLocal :: ModuleName.Raw -> Result -> Map.Map ModuleName.Raw Details.Local -> Map.Map ModuleName.Raw Details.Local
addNewLocal name result locals =
  case result of
    RNew  local _ _ _ -> Map.insert name local locals
    RSame local _ _ _ -> Map.insert name local locals
    RCached _ _ _     -> locals
    RNotFound _       -> locals
    RProblem _        -> locals
    RBlocked          -> locals
    RForeign _        -> locals
    RKernel           -> locals



-- FINALIZE EXPOSED


finalizeExposed :: DocsGoal docs -> NE.List ModuleName.Raw -> Map.Map ModuleName.Raw Result -> IO (Either Exit.BuildProblem docs)
finalizeExposed docsGoal exposed results =
  case foldr (addImportProblems results) [] (NE.toList exposed) of
    p:ps ->
      return $ Left $ Exit.BuildProjectProblem (Exit.BP_MissingExposed (NE.List p ps))

    [] ->
      case Map.foldr addErrors [] results of
        []   -> Right <$> finalizeDocs docsGoal results
        e:es -> return $ Left $ Exit.BuildBadModules e es


addErrors :: Result -> [Error.Module] -> [Error.Module]
addErrors result errors =
  case result of
    RNew  _ _ _ _ ->   errors
    RSame _ _ _ _ ->   errors
    RCached _ _ _ ->   errors
    RNotFound _   ->   errors
    RProblem e    -> e:errors
    RBlocked      ->   errors
    RForeign _    ->   errors
    RKernel       ->   errors


addImportProblems :: Map.Map ModuleName.Raw Result -> ModuleName.Raw -> [(ModuleName.Raw, Import.Problem)] -> [(ModuleName.Raw, Import.Problem)]
addImportProblems results name problems =
  case results ! name of
    RNew  _ _ _ _ -> problems
    RSame _ _ _ _ -> problems
    RCached _ _ _ -> problems
    RNotFound p   -> (name, p) : problems
    RProblem _    -> problems
    RBlocked      -> problems
    RForeign _    -> problems
    RKernel       -> problems



-- DOCS


data DocsGoal a where
  KeepDocs :: DocsGoal Docs.Documentation
  WriteDocs :: FilePath -> DocsGoal ()
  IgnoreDocs :: DocsGoal ()


newtype DocsNeed =
  DocsNeed { needsDocs :: Bool }


toDocsNeed :: DocsGoal a -> DocsNeed
toDocsNeed goal =
  case goal of
    IgnoreDocs  -> DocsNeed False
    WriteDocs _ -> DocsNeed True
    KeepDocs    -> DocsNeed True


makeDocs :: DocsNeed -> Can.Module -> Maybe Docs.Module
makeDocs (DocsNeed isNeeded) modul =
  if isNeeded then
    case Docs.fromModule modul of
      Right docs -> Just docs
      Left _     -> Nothing
  else
    Nothing


finalizeDocs :: DocsGoal docs -> Map.Map ModuleName.Raw Result -> IO docs
finalizeDocs goal results =
  case goal of
    KeepDocs ->
      return $ Map.mapMaybe toDocs results

    WriteDocs path ->
      E.writeUgly path $ Docs.encode $ Map.mapMaybe toDocs results

    IgnoreDocs ->
      return ()


toDocs :: Result -> Maybe Docs.Module
toDocs result =
  case result of
    RNew  _ _ _ d -> d
    RSame _ _ _ d -> d
    RCached _ _ _ -> Nothing
    RNotFound _   -> Nothing
    RProblem _    -> Nothing
    RBlocked      -> Nothing
    RForeign _    -> Nothing
    RKernel       -> Nothing



--------------------------------------------------------------------------------
------ NOW FOR SOME REPL STUFF -------------------------------------------------
--------------------------------------------------------------------------------


-- FROM REPL


data ReplArtifacts =
  ReplArtifacts
    { _repl_home :: ModuleName.Canonical
    , _repl_modules :: [Module]
    , _repl_localizer :: L.Localizer
    , _repl_annotations :: Map.Map Name.Name Can.Annotation
    }


fromRepl :: FilePath -> Details.Details -> B.ByteString -> IO (Either Exit.Repl ReplArtifacts)
fromRepl root details source =
  let
    env@(Env _ _ projectType _ _ _ _) = makeEnv Reporting.ignorer root details
  in
  case Parse.fromByteString projectType source of
    Left syntaxError ->
      return $ Left $ Exit.ReplBadInput source $ Error.BadSyntax syntaxError

    Right modul@(Src.Module _ _ _ imports _ _ _ _ _) ->
      do  dmvar <- Details.loadInterfaces root details

          let deps = map Src.getImportName imports
          mvar <- newMVar Map.empty
          crawlDeps env mvar deps ()

          statuses <- traverse readMVar =<< readMVar mvar
          midpoint <- checkMidpoint dmvar statuses

          case midpoint of
            Left problem ->
              return $ Left $ Exit.ReplProjectProblem problem

            Right foreigns ->
              do  rmvar <- newEmptyMVar
                  resultMVars <- forkWithKey (checkModule env foreigns rmvar) statuses
                  putMVar rmvar resultMVars
                  results <- traverse readMVar resultMVars
                  writeDetails root details results
                  depsStatus <- checkDeps root resultMVars deps 0
                  finalizeReplArtifacts env source modul depsStatus resultMVars results


finalizeReplArtifacts :: Env -> B.ByteString -> Src.Module -> DepsStatus -> ResultDict -> Map.Map ModuleName.Raw Result -> IO (Either Exit.Repl ReplArtifacts)
finalizeReplArtifacts env@(Env _ root projectType _ _ _ _) source modul@(Src.Module _ _ _ imports _ _ _ _ _) depsStatus resultMVars results =
  let
    pkg =
      projectTypeToPkg projectType

    compileInput ifaces =
      case Compile.compile pkg ifaces modul of
        Right (Compile.Artifacts canonical annotations objects) ->
          let
            h = Can._name canonical
            m = Fresh (Src.getName modul) (I.fromModule pkg canonical annotations) objects
            ms = Map.foldrWithKey addInside [] results
          in
          return $ Right $ ReplArtifacts h (m:ms) (L.fromModule modul) annotations

        Left errors ->
          return $ Left $ Exit.ReplBadInput source errors
  in
  case depsStatus of
    DepsChange ifaces ->
      compileInput ifaces

    DepsSame same cached ->
      do  maybeLoaded <- loadInterfaces root same cached
          case maybeLoaded of
            Just ifaces -> compileInput ifaces
            Nothing     -> return $ Left $ Exit.ReplBadCache

    DepsBlock ->
      case Map.foldr addErrors [] results of
        []   -> return $ Left $ Exit.ReplBlocked
        e:es -> return $ Left $ Exit.ReplBadLocalDeps e es

    DepsNotFound problems ->
      return $ Left $ Exit.ReplBadInput source $ Error.BadImports $
        toImportErrors env resultMVars imports problems



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
------ AFTER THIS, EVERYTHING IS ABOUT HANDLING MODULES GIVEN BY FILEPATH ------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------



-- FIND MAIN


data MainLocation
  = LInside ModuleName.Raw
  | LOutside FilePath


findMains :: Env -> NE.List FilePath -> IO (Either Exit.BuildProjectProblem (NE.List MainLocation))
findMains env paths =
  do  mvars <- traverse (fork . findLocation env) paths
      elocs <- traverse readMVar mvars
      return $ checkLocations =<< sequence elocs


checkLocations :: NE.List Location -> Either Exit.BuildProjectProblem (NE.List MainLocation)
checkLocations locations =
  let
    toOneOrMore loc@(Location absolute _ _) =
      (absolute, OneOrMore.one loc)

    fromOneOrMore loc locs =
      case locs of
        [] -> Right ()
        loc2:_ -> Left (Exit.BP_MainPathDuplicate (_relative loc) (_relative loc2))
  in
  fmap (\_ -> fmap _location locations) $
    traverse (OneOrMore.destruct fromOneOrMore) $
      Map.fromListWith OneOrMore.more $ map toOneOrMore (NE.toList locations)



-- LOCATIONS


data Location =
  Location
    { _absolute :: FilePath
    , _relative :: FilePath
    , _location :: MainLocation
    }


findLocation :: Env -> FilePath -> IO (Either Exit.BuildProjectProblem Location)
findLocation env path =
  do  exists <- File.exists path
      if exists
        then findLoc env path =<< Dir.canonicalizePath path
        else return (Left (Exit.BP_PathUnknown path))


findLoc :: Env -> FilePath -> FilePath -> IO (Either Exit.BuildProjectProblem Location)
findLoc (Env _ root _ srcDirs _ _ _) path absolutePath =
  let
    (dirs, file) = FP.splitFileName absolutePath
    (final, ext) = FP.splitExtension file
  in
  if ext /= ".elm"
  then
    return $ Left $ Exit.BP_WithBadExtension path
  else
    let
      roots = FP.splitDirectories root
      segments = FP.splitDirectories dirs ++ [final]
    in
    case dropPrefix roots segments of
      Nothing ->
        return $ Right $ Location absolutePath path (LOutside path)

      Just relativeSegments ->
        case Maybe.mapMaybe (isInsideSrcDirByPath relativeSegments) srcDirs of
          [] ->
            return $ Right $ Location absolutePath path (LOutside path)

          [(_, Right names)] ->
            do  let name = Name.fromChars (List.intercalate "." names)
                matchingDirs <- filterM (isInsideSrcDirByName root names) srcDirs
                case matchingDirs of
                  d1:d2:_ ->
                    do  let p1 = d1 </> FP.joinPath names <.> "elm"
                        let p2 = d2 </> FP.joinPath names <.> "elm"
                        return $ Left $ Exit.BP_MainNameDuplicate name p1 p2

                  _ ->
                    return $ Right $ Location absolutePath path (LInside name)

          [(s, Left names)] ->
            return $ Left $ Exit.BP_MainNameInvalid path s names

          (s1,_):(s2,_):_ ->
            return $ Left $ Exit.BP_WithAmbiguousSrcDir path s1 s2



isInsideSrcDirByName :: FilePath -> [String] -> FilePath -> IO Bool
isInsideSrcDirByName root names srcDir =
  File.exists (root </> srcDir </> FP.joinPath names <.> "elm")


isInsideSrcDirByPath :: [String] -> FilePath -> Maybe (FilePath, Either [String] [String])
isInsideSrcDirByPath segments srcDir =
  case dropPrefix (FP.splitDirectories srcDir) segments of
    Nothing ->
      Nothing

    Just names ->
      if all isGoodName names
      then Just (srcDir, Right names)
      else Just (srcDir, Left names)


isGoodName :: [Char] -> Bool
isGoodName name =
  case name of
    [] ->
      False

    char:chars ->
      Char.isUpper char && all (\c -> Char.isAlphaNum c || c == '_') chars


dropPrefix :: [FilePath] -> [FilePath] -> Maybe [FilePath]
dropPrefix roots paths =
  case roots of
    [] ->
      Just paths

    ".":rs ->
      dropPrefix rs paths

    r:rs ->
      case paths of
        []     -> Nothing
        ".":ps -> dropPrefix roots ps
        p:ps   -> if r == p then dropPrefix rs ps else Nothing



-- CRAWL MAINS


data MainStatus
  = SInside ModuleName.Raw
  | SOutsideOk Details.Local B.ByteString Src.Module
  | SOutsideErr Error.Module


crawlMain :: Env -> MVar StatusDict -> MainLocation -> IO MainStatus
crawlMain env@(Env _ _ projectType _ buildID _ _) mvar given =
  case given of
    LInside name ->
      do  statusMVar <- newEmptyMVar
          statusDict <- takeMVar mvar
          putMVar mvar (Map.insert name statusMVar statusDict)
          putMVar statusMVar =<< crawlModule env mvar (DocsNeed False) name
          return (SInside name)

    LOutside path ->
      do  time <- File.getTime path
          source <- File.readUtf8 path
          case Parse.fromByteString projectType source of
            Right modul@(Src.Module _ _ _ imports values _ _ _ _) ->
              do  let deps = map Src.getImportName imports
                  let local = Details.Local path time deps (any isMain values) buildID buildID
                  crawlDeps env mvar deps (SOutsideOk local source modul)

            Left syntaxError ->
              return $ SOutsideErr $
                Error.Module "???" path time source (Error.BadSyntax syntaxError)



-- CHECK MAINS


data MainResult
  = RInside ModuleName.Raw
  | ROutsideOk ModuleName.Raw I.Interface Opt.LocalGraph
  | ROutsideErr Error.Module
  | ROutsideBlocked


checkMain :: Env -> ResultDict -> MainStatus -> IO MainResult
checkMain env@(Env _ root _ _ _ _ _) results pendingMain =
  case pendingMain of
    SInside name ->
      return (RInside name)

    SOutsideErr err ->
      return (ROutsideErr err)

    SOutsideOk local@(Details.Local path time deps _ _ lastCompile) source modul@(Src.Module _ _ _ imports _ _ _ _ _) ->
      do  depsStatus <- checkDeps root results deps lastCompile
          case depsStatus of
            DepsChange ifaces ->
              compileOutside env local source ifaces modul

            DepsSame same cached ->
              do  maybeLoaded <- loadInterfaces root same cached
                  case maybeLoaded of
                    Nothing     -> return ROutsideBlocked
                    Just ifaces -> compileOutside env local source ifaces modul

            DepsBlock ->
              return ROutsideBlocked

            DepsNotFound problems ->
              return $ ROutsideErr $ Error.Module (Src.getName modul) path time source $
                  Error.BadImports (toImportErrors env results imports problems)


compileOutside :: Env -> Details.Local -> B.ByteString -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> IO MainResult
compileOutside (Env key _ projectType _ _ _ _) (Details.Local path time _ _ _ _) source ifaces modul =
  let
    pkg = projectTypeToPkg projectType
    name = Src.getName modul
  in
  case Compile.compile pkg ifaces modul of
    Right (Compile.Artifacts canonical annotations objects) ->
      do  Reporting.report key Reporting.BDone
          return $ ROutsideOk name (I.fromModule pkg canonical annotations) objects

    Left errors ->
      return $ ROutsideErr $ Error.Module name path time source errors



-- TO ARTIFACTS


data Main
  = Inside ModuleName.Raw
  | Outside ModuleName.Raw I.Interface Opt.LocalGraph


toArtifacts :: Env -> Dependencies -> Map.Map ModuleName.Raw Result -> NE.List MainResult -> Either Exit.BuildProblem Artifacts
toArtifacts (Env _ _ projectType _ _ _ _) foreigns results mainResults =
  case gatherProblemsOrMains results mainResults of
    Left (NE.List e es) ->
      Left (Exit.BuildBadModules e es)

    Right mains ->
      Right $ Artifacts (projectTypeToPkg projectType) foreigns mains $
        Map.foldrWithKey addInside (foldr addOutside [] mainResults) results


gatherProblemsOrMains :: Map.Map ModuleName.Raw Result -> NE.List MainResult -> Either (NE.List Error.Module) (NE.List Main)
gatherProblemsOrMains results (NE.List mainResult mainResults) =
  let
    sortMain result (es, mains) =
      case result of
        RInside n        -> (  es, Inside n      : mains)
        ROutsideOk n i o -> (  es, Outside n i o : mains)
        ROutsideErr e    -> (e:es,                 mains)
        ROutsideBlocked  -> (  es,                 mains)

    errors = Map.foldr addErrors [] results
  in
  case (mainResult, foldr sortMain (errors, []) mainResults) of
    (RInside n       , (  [], ms)) -> Right (NE.List (Inside n) ms)
    (RInside _       , (e:es, _ )) -> Left  (NE.List e es)
    (ROutsideOk n i o, (  [], ms)) -> Right (NE.List (Outside n i o) ms)
    (ROutsideOk _ _ _, (e:es, _ )) -> Left  (NE.List e es)
    (ROutsideErr e   , (  es, _ )) -> Left  (NE.List e es)
    (ROutsideBlocked , (  [], _ )) -> error "seems like elm-stuff/ is corrupted"
    (ROutsideBlocked , (e:es, _ )) -> Left  (NE.List e es)


addInside :: ModuleName.Raw -> Result -> [Module] -> [Module]
addInside name result modules =
  case result of
    RNew  _ iface objs _ -> Fresh name iface objs : modules
    RSame _ iface objs _ -> Fresh name iface objs : modules
    RCached main _ mvar  -> Cached name main mvar : modules
    RNotFound _          -> error (badInside name)
    RProblem _           -> error (badInside name)
    RBlocked             -> error (badInside name)
    RForeign _           -> modules
    RKernel              -> modules


badInside :: ModuleName.Raw -> [Char]
badInside name =
  "Error from `" ++ Name.toChars name ++ "` should have been reported already."


addOutside :: MainResult -> [Module] -> [Module]
addOutside main modules =
  case main of
    RInside _                  -> modules
    ROutsideOk name iface objs -> Fresh name iface objs : modules
    ROutsideErr _              -> modules
    ROutsideBlocked            -> modules
