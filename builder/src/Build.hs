{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE BangPatterns #-}
module Build
  ( fromExposed
  , fromMains
  , Artifacts(..)
  , Main(..)
  , Module(..)
  , CachedInterface(..)
  )
  where


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (filterM, mapM_, sequence_)
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map.Utils as Map
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE
import qualified Data.OneOrMore as OneOrMore
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import System.FilePath ((</>), (<.>))

import qualified AST.Source as Src
import qualified AST.Optimized as Opt
import qualified Compile
import qualified Elm.Details as Details
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified File
import qualified Parse.Module as Parse
import qualified Reporting
import qualified Reporting.Annotation as A
import qualified Reporting.Exit as Exit
import qualified Reporting.Problem as Problem
import qualified Reporting.Render.Type.Localizer as Localizer
import qualified Stuff



-- ENVIRONMENT


data Env =
  Env
    { _key :: Reporting.BKey
    , _root :: FilePath
    , _pkg :: Pkg.Name
    , _srcDirs :: [FilePath]
    , _locals :: Map.Map ModuleName.Raw Details.Local
    , _foreigns :: Map.Map ModuleName.Raw Details.Foreign
    }


makeEnv :: Reporting.BKey -> FilePath -> Details.Details -> Env
makeEnv key root (Details.Details _ validOutline locals foreigns) =
  case validOutline of
    Details.ValidApp (Outline.AppOutline _ srcDirs _ _ _ _) ->
      Env key root Pkg.dummyName srcDirs locals foreigns

    Details.ValidPkg (Outline.PkgOutline pkg _ _ _ _ _ _ _) _ ->
      Env key root pkg ["src"] locals foreigns



-- FORK


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


fromExposed :: Reporting.Style -> FilePath -> Details.Details -> NE.List ModuleName.Raw -> IO (Either Exit.BuildProblem ())
fromExposed style root details (NE.List e es) =
  Reporting.trackBuild style $ \key ->
  do  let env = makeEnv key root details
      dmvar <- Details.loadInterfaces root details

      -- crawl
      mvar <- newEmptyMVar
      roots <- Map.fromKeysA (fork . crawlModule env mvar) (e:es)
      putMVar mvar roots
      mapM_ readMVar roots
      statuses <- traverse readMVar =<< readMVar mvar

      -- compile
      midpoint <- checkExposedMidpoint dmvar statuses
      case midpoint of
        Left problem ->
          return (Left (Exit.BuildProjectProblem problem))

        Right foreigns ->
          do  rmvar <- newEmptyMVar
              resultMVars <- forkWithKey (checkModule env foreigns rmvar) statuses
              putMVar rmvar resultMVars
              results <- traverse readMVar resultMVars
              writeDetails root details results
              return (detectProblems results)



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



-- CRAWL


type StatusDict =
  Map.Map ModuleName.Raw (MVar Status)


data Status
  = SCached Details.Local
  | SChanged Details.Local Src.Module
  | SNotFound Problem.Find
  | SNotValid Problem.Syntax
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
    crawlNew name () = fork (crawlModule env mvar name)


crawlModule :: Env -> MVar StatusDict -> ModuleName.Raw -> IO Status
crawlModule env@(Env _ root pkg srcDirs locals foreigns) mvar name =
  do  let fileName = ModuleName.toFilePath name <.> "elm"
      let inRoot path = File.exists (root </> path)

      paths <- filterM inRoot (map (</> fileName) srcDirs)

      case paths of
        [path] ->
          case Map.lookup name foreigns of
            Just (Details.Foreign dep deps) ->
              return $ SNotFound $ Problem.Ambiguous path [] dep deps

            Nothing ->
              case Map.lookup name locals of
                Nothing ->
                  crawlFile env mvar name path

                Just local@(Details.Local oldPath oldTime deps _) ->
                  if path /= oldPath
                  then crawlFile env mvar name path
                  else
                    do  newTime <- File.getTime path
                        if oldTime < newTime
                          then crawlFile env mvar name path
                          else crawlDeps env mvar deps (SCached local)

        p1:p2:ps ->
          return $ SNotFound $ Problem.AmbiguousLocal p1 p2 ps

        [] ->
          case Map.lookup name foreigns of
            Just (Details.Foreign dep deps) ->
              case deps of
                [] ->
                  return $ SForeign dep

                d:ds ->
                  return $ SNotFound $ Problem.AmbiguousForeign dep d ds

            Nothing ->
              if Name.isKernel name && Pkg.isKernel pkg then
                do  exists <- File.exists ("src" </> ModuleName.toFilePath name <.> "js")
                    return $ if exists then SKernel else SNotFound Problem.NotFound
              else
                return $ SNotFound Problem.NotFound


crawlFile :: Env -> MVar StatusDict -> ModuleName.Raw -> FilePath -> IO Status
crawlFile env@(Env _ root pkg _ _ _) mvar expectedName path =
  do  time <- File.getTime path
      result <- Parse.fromFile pkg (root </> path)

      case result of
        Right modul@(Src.Module maybeActualName _ imports values _ _ _ _) ->
          case maybeActualName of
            Just actualName ->
              if expectedName == actualName then
                let
                  deps = map Src.getImportName imports
                  local = Details.Local path time deps (any isMain values)
                in
                crawlDeps env mvar deps (SChanged local modul)
              else
                return $ SNotValid $ Problem.BadModuleName_Mismatch expectedName actualName

            Nothing ->
              return $ SNotValid $ Problem.BadModuleName_Unspecified expectedName

        Left syntaxError ->
          return $ SNotValid $ Problem.SyntaxProblem path time syntaxError


isMain :: A.Located Src.Value -> Bool
isMain (A.At _ (Src.Value (A.At _ name) _ _ _)) =
  name == Name.main



-- CHECK MODULE


type ResultDict =
  Map.Map ModuleName.Raw (MVar Result)


data Result
  = RNew Details.Local I.Interface Opt.LocalGraph
  | RSame Details.Local I.Interface Opt.LocalGraph
  | RCached Bool (MVar CachedInterface)
  | RProblem Problem.Problem
  | RBlocked
  | RForeign I.Interface
  | RKernel


data CachedInterface
  = Unneeded
  | Loaded I.Interface
  | Corrupted


checkModule :: Env -> Dependencies -> MVar ResultDict -> ModuleName.Raw -> Status -> IO Result
checkModule env@(Env _ root pkg _ _ _) foreigns resultsMVar name status =
  case status of
    SCached local@(Details.Local path time deps hasMain) ->
      do  results <- readMVar resultsMVar
          depsStatus <- checkDeps root results deps
          case depsStatus of
            DepsChange ifaces ->
              do  result <- Parse.fromFile pkg path
                  case result of
                    Right modul -> compile env local ifaces modul
                    Left syntaxError ->
                      return $ RProblem $ Problem.BadSyntax $
                        Problem.SyntaxProblem path time syntaxError

            DepsSame _ _ ->
              do  mvar <- newMVar Unneeded
                  return (RCached hasMain mvar)

            DepsBlock ->
              return RBlocked

    SChanged local@(Details.Local _ _ deps _) modul ->
      do  results <- readMVar resultsMVar
          depsStatus <- checkDeps root results deps
          case depsStatus of
            DepsChange ifaces ->
              compile env local ifaces modul

            DepsSame same cached ->
              do  maybeLoaded <- checkCachedInterfaces root cached
                  case maybeLoaded of
                    Nothing ->
                      return RBlocked

                    Just loaded ->
                      do  let ifaces = Map.union loaded (Map.fromList same)
                          compile env local ifaces modul

            DepsBlock ->
              return RBlocked

    SNotFound findProblem ->
      return (RProblem (Problem.BadFind findProblem))

    SNotValid syntaxProblem ->
      return (RProblem (Problem.BadSyntax syntaxProblem))

    SForeign home ->
      case foreigns ! ModuleName.Canonical home name of
        I.Public iface -> return (RForeign iface)
        I.Private _ _ _ -> error "loading private interface in Build"

    SKernel ->
      return RKernel



-- CHECK DEPS


data DepsStatus
  = DepsChange (Map.Map ModuleName.Raw I.Interface)
  | DepsSame [Dep] [CDep]
  | DepsBlock


checkDeps :: FilePath -> ResultDict -> [ModuleName.Raw] -> IO DepsStatus
checkDeps root results deps =
  checkDepsHelp root results deps [] [] []


type Dep = (ModuleName.Raw, I.Interface)
type CDep = (ModuleName.Raw, MVar CachedInterface)


checkDepsHelp :: FilePath -> ResultDict -> [ModuleName.Raw] -> [Dep] -> [Dep] -> [CDep] -> IO DepsStatus
checkDepsHelp root results deps new same cached =
  case deps of
    [] ->
      if null new
      then return (DepsSame same cached)
      else
        do  maybeLoaded <- checkCachedInterfaces root cached
            case maybeLoaded of
              Nothing ->
                return DepsBlock

              Just loaded ->
                return $ DepsChange $
                  Map.union loaded (Map.union (Map.fromList new) (Map.fromList same))

    dep:otherDeps ->
      do  result <- readMVar (results ! dep)
          case result of
            RNew _ iface _ ->
              checkDepsHelp root results otherDeps ((dep,iface) : new) same cached

            RSame _ iface _ ->
              checkDepsHelp root results otherDeps new ((dep,iface) : same) cached

            RCached _ mvar ->
              checkDepsHelp root results otherDeps new same ((dep,mvar) : cached)

            RProblem _ ->
              return DepsBlock

            RBlocked ->
              return DepsBlock

            RForeign iface ->
              checkDepsHelp root results otherDeps new ((dep,iface) : same) cached

            RKernel ->
              checkDepsHelp root results otherDeps new same cached



-- CACHED INTERFACE


checkCachedInterfaces :: FilePath -> [(ModuleName.Raw, MVar CachedInterface)] -> IO (Maybe (Map.Map ModuleName.Raw I.Interface))
checkCachedInterfaces root deps =
  do  loading <- traverse (fork . checkCache root) deps
      loaded <- traverse readMVar loading
      return $ Map.fromList <$> sequence loaded


checkCache :: FilePath -> (ModuleName.Raw, MVar CachedInterface) -> IO (Maybe Dep)
checkCache root (name, ciMvar) =
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


checkExposedMidpoint :: MVar (Maybe Dependencies) -> Map.Map ModuleName.Raw Status -> IO (Either Exit.BuildProjectProblem Dependencies)
checkExposedMidpoint dmvar statuses =
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
        SCached  (Details.Local _ _ deps _)   -> deps
        SChanged (Details.Local _ _ deps _) _ -> deps
        SNotFound _                           -> []
        SNotValid _                           -> []
        SForeign _                            -> []
        SKernel                               -> []
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
    SInside _                                   -> Nothing
    SOutsideOk (Details.Local path _ _ _) modul -> Just (Src.getName modul, OneOrMore.one path)
    SOutsideErr _                               -> Nothing


checkOutside :: ModuleName.Raw -> OneOrMore.OneOrMore FilePath -> Either Exit.BuildProjectProblem FilePath
checkOutside name paths =
  case OneOrMore.destruct NE.List paths of
    NE.List p  []     -> Right p
    NE.List p1 (p2:_) -> Left (Exit.BP_MainNameDuplicate name p1 p2)


checkInside :: ModuleName.Raw -> FilePath -> Status -> Either Exit.BuildProjectProblem ()
checkInside name p1 status =
  case status of
    SCached  (Details.Local p2 _ _ _)   -> Left (Exit.BP_MainNameDuplicate name p1 p2)
    SChanged (Details.Local p2 _ _ _) _ -> Left (Exit.BP_MainNameDuplicate name p1 p2)
    SNotFound _                         -> Right ()
    SNotValid _                         -> Right ()
    SForeign _                          -> Right ()
    SKernel                             -> Right ()



-- COMPILE MODULE


compile :: Env -> Details.Local -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> IO Result
compile (Env key root pkg _ _ _) local@(Details.Local path time _ _) ifaces modul =
  case Compile.compile pkg ifaces modul of
    Right (Compile.Artifacts canonical annotations objects) ->
      do  let name = Src.getName modul
          let iface = I.fromModule pkg canonical annotations
          File.writeBinary (Stuff.elmo root name) objects
          maybeOldi <- File.readBinary (Stuff.elmi root name)
          case maybeOldi of
            Just oldi | oldi == iface ->
              do  -- iface should be fully forced by equality check
                  Reporting.report key Reporting.BDone
                  return (RSame local iface objects)

            _ ->
              do  -- iface may be lazy still
                  -- TODO try adding forkIO here
                  File.writeBinary (Stuff.elmi root name) iface
                  Reporting.report key Reporting.BDone
                  return (RNew local iface objects)

    Left errors ->
      do  Reporting.report key Reporting.BDone
          return $ RProblem $ Problem.BadContent $
            Problem.Module (Src.getName modul) path time errors (Localizer.fromModule modul)



-- WRITE DETAILS


writeDetails :: FilePath -> Details.Details -> Map.Map ModuleName.Raw Result -> IO ()
writeDetails root (Details.Details time outline locals foreigns) results =
  File.writeBinary (Stuff.details root) $
    Details.Details time outline (Map.foldrWithKey addNewLocal locals results) foreigns


addNewLocal :: ModuleName.Raw -> Result -> Map.Map ModuleName.Raw Details.Local -> Map.Map ModuleName.Raw Details.Local
addNewLocal name result locals =
  case result of
    RNew  local _ _ -> Map.insert name local locals
    RSame local _ _ -> Map.insert name local locals
    RCached _ _     -> locals
    RProblem _      -> locals
    RBlocked        -> locals
    RForeign _      -> locals
    RKernel         -> locals



-- DETECT PROBLEMS


detectProblems :: Map.Map ModuleName.Raw Result -> Either Exit.BuildProblem ()
detectProblems results =
  case Map.foldr addProblem [] results of
    []   -> Right ()
    p:ps -> Left (Exit.BuildModuleProblems p ps)


addProblem :: Result -> [Problem.Problem] -> [Problem.Problem]
addProblem result problems =
  case result of
    RNew  _ _ _ ->   problems
    RSame _ _ _ ->   problems
    RCached _ _ ->   problems
    RProblem p  -> p:problems
    RBlocked    ->   problems
    RForeign _  ->   problems
    RKernel     ->   problems



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
        then isInsideSrcDirs env path <$> Dir.canonicalizePath path
        else return (Left (Exit.BP_PathUnknown path))


isInsideSrcDirs :: Env -> FilePath -> FilePath -> Either Exit.BuildProjectProblem Location
isInsideSrcDirs (Env _ root _ srcDirs _ _) path absolutePath =
  let
    (dirs, file) = FP.splitFileName absolutePath
    (final, ext) = break (=='.') file
  in
  if ext /= ".elm"
  then Left (Exit.BP_WithBadExtension path)
  else
    let
      roots = FP.splitDirectories root
      segments = FP.splitDirectories dirs ++ [final]
    in
    case dropPrefix roots segments of
      Nothing ->
        Right (Location absolutePath path (LOutside path))

      Just relativeSegments ->
        let
          (exits, maybes) =
            Either.partitionEithers (map (isInsideSrcDirsHelp relativeSegments) srcDirs)
        in
        case (exits, Maybe.catMaybes maybes) of
          (_, [(_,name)])      -> Right (Location absolutePath path (LInside name))
          ([], [])             -> Right (Location absolutePath path (LOutside path))
          (_, (s1,_):(s2,_):_) -> Left (Exit.BP_WithAmbiguousSrcDir s1 s2)
          (exit:_, _)          -> Left exit


isInsideSrcDirsHelp :: [String] -> FilePath -> Either Exit.BuildProjectProblem (Maybe (FilePath, ModuleName.Raw))
isInsideSrcDirsHelp segments srcDir =
  case dropPrefix (FP.splitDirectories srcDir) segments of
    Nothing ->
      Right Nothing

    Just names ->
      if all isGoodName names
      then Right (Just (srcDir, Name.fromChars (List.intercalate "." names)))
      else Left (error "Exit.MakeWithInvalidModuleName" srcDir segments)


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

    r:rs ->
      case paths of
        [] -> Nothing
        p:ps -> if r == p then dropPrefix rs ps else Nothing



-- CRAWL MAINS


data MainStatus
  = SInside ModuleName.Raw
  | SOutsideOk Details.Local Src.Module
  | SOutsideErr Problem.Syntax


crawlMain :: Env -> MVar StatusDict -> MainLocation -> IO MainStatus
crawlMain env@(Env _ _ pkg _ _ _) mvar given =
  case given of
    LInside name ->
      do  statusMVar <- newEmptyMVar
          statusDict <- takeMVar mvar
          putMVar mvar (Map.insert name statusMVar statusDict)
          putMVar statusMVar =<< crawlModule env mvar name
          return (SInside name)

    LOutside path ->
      do  time <- File.getTime path
          result <- Parse.fromFile pkg path
          case result of
            Right modul@(Src.Module _ _ imports values _ _ _ _) ->
              do  let deps = map Src.getImportName imports
                  let local = Details.Local path time deps (any isMain values)
                  crawlDeps env mvar deps (SOutsideOk local modul)

            Left syntaxError ->
              return (SOutsideErr (Problem.SyntaxProblem path time syntaxError))



-- CHECK MAINS


data MainResult
  = RInside ModuleName.Raw
  | ROutsideOk ModuleName.Raw I.Interface Opt.LocalGraph
  | ROutsideErr Problem.Problem
  | ROutsideBlocked


checkMain :: Env -> ResultDict -> MainStatus -> IO MainResult
checkMain env@(Env _ root _ _ _ _) results pendingMain =
  case pendingMain of
    SInside name ->
      return (RInside name)

    SOutsideErr syntaxProblem ->
      return (ROutsideErr (Problem.BadSyntax syntaxProblem))

    SOutsideOk local@(Details.Local _ _ deps _) modul ->
      do  depsStatus <- checkDeps root results deps
          case depsStatus of
            DepsChange ifaces ->
              return $ compileOutside env local ifaces modul

            DepsSame same cached ->
              do  maybeLoaded <- checkCachedInterfaces root cached
                  case maybeLoaded of
                    Nothing ->
                      return ROutsideBlocked

                    Just loaded ->
                      do  let ifaces = Map.union loaded (Map.fromList same)
                          return $ compileOutside env local ifaces modul

            DepsBlock ->
              return ROutsideBlocked


compileOutside :: Env -> Details.Local -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> MainResult
compileOutside (Env _ _ pkg _ _ _) (Details.Local path time _ _) ifaces modul =
  let
    name = Src.getName modul
  in
  case Compile.compile pkg ifaces modul of
    Right (Compile.Artifacts canonical annotations objects) ->
      ROutsideOk name (I.fromModule pkg canonical annotations) objects

    Left errors ->
      ROutsideErr $ Problem.BadContent $
        Problem.Module name path time errors (Localizer.fromModule modul)



-- TO ARTIFACTS


data Main
  = Inside ModuleName.Raw
  | Outside ModuleName.Raw I.Interface Opt.LocalGraph


toArtifacts :: Env -> Dependencies -> Map.Map ModuleName.Raw Result -> NE.List MainResult -> Either Exit.BuildProblem Artifacts
toArtifacts (Env _ _ pkg _ _ _) foreigns results mainResults =
  case gatherProblemsOrMains results mainResults of
    Left (NE.List p ps) ->
      Left (Exit.BuildModuleProblems p ps)

    Right mains ->
      Right $ Artifacts pkg foreigns mains $
        Map.foldrWithKey addInside (foldr addOutside [] mainResults) results


gatherProblemsOrMains :: Map.Map ModuleName.Raw Result -> NE.List MainResult -> Either (NE.List Problem.Problem) (NE.List Main)
gatherProblemsOrMains results (NE.List mainResult mainResults) =
  let
    sortMain result (ps, mains) =
      case result of
        RInside n        -> (  ps, Inside n      : mains)
        ROutsideOk n i o -> (  ps, Outside n i o : mains)
        ROutsideErr p    -> (p:ps,                 mains)
        ROutsideBlocked  -> (  ps,                 mains)

    problems = Map.foldr addProblem [] results
  in
  case (mainResult, foldr sortMain (problems, []) mainResults) of
    (RInside n       , (  [], ms)) -> Right (NE.List (Inside n) ms)
    (RInside _       , (p:ps, _ )) -> Left  (NE.List p ps)
    (ROutsideOk n i o, (  [], ms)) -> Right (NE.List (Outside n i o) ms)
    (ROutsideOk _ _ _, (p:ps, _ )) -> Left  (NE.List p ps)
    (ROutsideErr p   , (  ps, _ )) -> Left  (NE.List p ps)
    (ROutsideBlocked , (  [], _ )) -> error "seems like elm-stuff/ is corrupted"
    (ROutsideBlocked , (p:ps, _ )) -> Left  (NE.List p ps)


addInside :: ModuleName.Raw -> Result -> [Module] -> [Module]
addInside name result modules =
  case result of
    RNew  _ iface objs -> Fresh name iface objs : modules
    RSame _ iface objs -> Fresh name iface objs : modules
    RCached main mvar  -> Cached name main mvar : modules
    RProblem _         -> error ("I missed the error in `" ++ Name.toChars name ++ "` module.")
    RBlocked           -> error ("I missed the error in `" ++ Name.toChars name ++ "` module.")
    RForeign _         -> modules
    RKernel            -> modules


addOutside :: MainResult -> [Module] -> [Module]
addOutside main modules =
  case main of
    RInside _                  -> modules
    ROutsideOk name iface objs -> Fresh name iface objs : modules
    ROutsideErr _              -> modules
    ROutsideBlocked            -> modules
