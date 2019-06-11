{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Make
  ( Flags(..)
  , Output(..)
  , ReportType(..)
  , run
  , reportType
  , output
  , docsFile
  )
  where


import Control.Concurrent (MVar, forkIO, newEmptyMVar, newMVar, putMVar, readMVar)
import Control.Monad (liftM2)
import qualified Data.ByteString.Builder as B
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.NonEmptyList as NE
import qualified System.FilePath as FP

import qualified AST.Optimized as Opt
import qualified Build
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Elm.Details as Details
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified File
import qualified Generate.Html as Html
import qualified Generate.JavaScript as Generate
import qualified Generate.Mode as Mode
import qualified Nitpick.Debug as Nitpick
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff
import Terminal.Args (Parser(..))



-- FLAGS


data Flags =
  Flags
    { _debug :: Bool
    , _optimize :: Bool
    , _output :: Maybe Output
    , _report :: Maybe ReportType
    , _docs :: Maybe FilePath
    }


data Output
  = JS FilePath
  | Html FilePath
  | DevNull


data ReportType
  = Json



-- RUN


type Task a = Task.Task Exit.Make a


run :: [FilePath] -> Flags -> IO ()
run paths (Flags debug optimize maybeOutput report _) =
  do  style <- getStyle report
      Reporting.attemptWithStyle style Exit.makeToReport $ Task.run $
        do  root <- getRoot
            desiredMode <- getMode debug optimize
            details <- getDetails style root
            case paths of
              [] ->
                do  exposed <- getExposed details
                    _ <- buildExposed style root details exposed
                    return ()

              p:ps ->
                do  artifacts <- buildPaths style root details (NE.List p ps)
                    case maybeOutput of
                      Nothing ->
                        case getMains artifacts of
                          [] ->
                            return ()

                          [name] ->
                            do  builder <- toBuilder root details desiredMode artifacts
                                generate style "index.html" (Html.sandwich name builder) (NE.List name [])

                          name:names ->
                            do  builder <- toBuilder root details desiredMode artifacts
                                generate style "elm.js" builder (NE.List name names)

                      Just DevNull ->
                        return ()

                      Just (JS target) ->
                        case getNoMains artifacts of
                          [] ->
                            do  builder <- toBuilder root details desiredMode artifacts
                                generate style target builder (getMainNames artifacts)

                          name:names ->
                            Task.throw (Exit.MakeNonMainFilesIntoJavaScript name names)

                      Just (Html target) ->
                        case hasOneMain artifacts of
                          Just name ->
                            do  builder <- toBuilder root details desiredMode artifacts
                                generate style target (Html.sandwich name builder) (NE.List name [])

                          Nothing ->
                            Task.throw Exit.MakeMultipleFilesIntoHtml



-- GET INFORMATION


getStyle :: Maybe ReportType -> IO Reporting.Style
getStyle report =
  case report of
    Nothing -> Reporting.terminal
    Just Json -> return Reporting.json


getRoot :: Task FilePath
getRoot =
  do  maybeRoot <- Task.io Stuff.findRoot
      case maybeRoot of
        Nothing -> Task.throw Exit.MakeNeedsOutline
        Just root -> return root


getMode :: Bool -> Bool -> Task DesiredMode
getMode debug optimize =
  case (debug, optimize) of
    (True , True ) -> Task.throw Exit.MakeCannotOptimizeAndDebug
    (True , False) -> return Debug
    (False, False) -> return Dev
    (False, True ) -> return Prod


getDetails :: Reporting.Style -> FilePath -> Task Details.Details
getDetails style root =
  Task.eio Exit.MakeBlockedByDetailsProblem (Details.load style root)


getExposed :: Details.Details -> Task (NE.List ModuleName.Raw)
getExposed (Details.Details _ validOutline _ _ _) =
  case validOutline of
    Details.ValidApp _ ->
      Task.throw Exit.MakeAppNeedsFileNames

    Details.ValidPkg _ exposed ->
      case exposed of
        [] -> Task.throw Exit.MakePkgNeedsExposing
        m:ms -> return (NE.List m ms)



-- BUILD PROJECTS


buildExposed :: Reporting.Style -> FilePath -> Details.Details -> NE.List ModuleName.Raw -> Task ()
buildExposed style root details exposed =
  Task.eio Exit.MakeCannotBuild $
    Build.fromExposed style root details Build.IgnoreDocs exposed


buildPaths :: Reporting.Style -> FilePath -> Details.Details -> NE.List FilePath -> Task Build.Artifacts
buildPaths style root details paths =
  Task.eio Exit.MakeCannotBuild $
    Build.fromMains style root details paths



-- GET MAINS


getMains :: Build.Artifacts -> [ModuleName.Raw]
getMains (Build.Artifacts _ _ mains modules) =
  Maybe.mapMaybe (getMain modules) (NE.toList mains)


getMain :: [Build.Module] -> Build.Main -> Maybe ModuleName.Raw
getMain modules main =
  case main of
    Build.Inside name ->
      if any (isMain name) modules
      then Just name
      else Nothing

    Build.Outside name _ (Opt.LocalGraph maybeMain _ _) ->
      case maybeMain of
        Just _  -> Just name
        Nothing -> Nothing


isMain :: ModuleName.Raw -> Build.Module -> Bool
isMain targetName modul =
  case modul of
    Build.Fresh name _ (Opt.LocalGraph maybeMain _ _) ->
      Maybe.isJust maybeMain && name == targetName

    Build.Cached name mainIsDefined _ ->
      mainIsDefined && name == targetName



-- HAS ONE MAIN


hasOneMain :: Build.Artifacts -> Maybe ModuleName.Raw
hasOneMain (Build.Artifacts _ _ mains modules) =
  case mains of
    NE.List main [] -> getMain modules main
    NE.List _ (_:_) -> Nothing



-- GET MAINLESS


getNoMains :: Build.Artifacts -> [ModuleName.Raw]
getNoMains (Build.Artifacts _ _ mains modules) =
  Maybe.mapMaybe (getNoMain modules) (NE.toList mains)


getNoMain :: [Build.Module] -> Build.Main -> Maybe ModuleName.Raw
getNoMain modules main =
  case main of
    Build.Inside name ->
      if any (isMain name) modules
      then Nothing
      else Just name

    Build.Outside name _ (Opt.LocalGraph maybeMain _ _) ->
      case maybeMain of
        Just _  -> Nothing
        Nothing -> Just name



-- GET MAIN NAMES


getMainNames :: Build.Artifacts -> NE.List ModuleName.Raw
getMainNames (Build.Artifacts _ _ mains _) =
  fmap getMainName mains


getMainName :: Build.Main -> ModuleName.Raw
getMainName main =
  case main of
    Build.Inside  name     -> name
    Build.Outside name _ _ -> name



-- GENERATE


generate :: Reporting.Style -> FilePath -> B.Builder -> NE.List ModuleName.Raw -> Task ()
generate style target builder names =
  Task.io $
    do  File.writeBuilder target builder
        Reporting.reportGenerate style names target



-- TO BUILDER


data DesiredMode = Debug | Dev | Prod


toBuilder :: FilePath -> Details.Details -> DesiredMode -> Build.Artifacts -> Task B.Builder
toBuilder root details desiredMode (Build.Artifacts pkg ifaces roots modules) =
  case desiredMode of
    Debug ->
      do  loading <- loadObjects root details modules
          types   <- loadTypes root ifaces modules
          objects <- finalizeObjects loading
          let mode = Mode.Dev (Just types)
          let graph = toGlobalGraph objects
          let mains = gatherMains pkg objects roots
          return $ Generate.generate mode graph mains

    Dev ->
      do  objects <- finalizeObjects =<< loadObjects root details modules
          let mode = Mode.Dev Nothing
          let graph = toGlobalGraph objects
          let mains = gatherMains pkg objects roots
          return $ Generate.generate mode graph mains

    Prod ->
      do  objects <- finalizeObjects =<< loadObjects root details modules
          checkForDebugUses objects
          let graph = toGlobalGraph objects
          let mode = Mode.Prod (Mode.shortenFieldNames graph)
          let mains = gatherMains pkg objects roots
          return $ Generate.generate mode graph mains


checkForDebugUses :: LoadedObjects -> Task ()
checkForDebugUses (LoadedObjects _ locals) =
  case Map.keys (Map.filter Nitpick.hasDebugUses locals) of
    []   -> return ()
    m:ms -> Task.throw (Exit.MakeCannotOptimizeDebugValues m ms)


toGlobalGraph :: LoadedObjects -> Opt.GlobalGraph
toGlobalGraph (LoadedObjects globals locals) =
  foldr Opt.addLocalGraph globals locals



-- LOAD OBJECTS


data LoadingObjects =
  LoadingObjects
    { _foreign_mvar :: MVar (Maybe Opt.GlobalGraph)
    , _local_mvars :: Map.Map ModuleName.Raw (MVar (Maybe Opt.LocalGraph))
    }


loadObjects :: FilePath -> Details.Details -> [Build.Module] -> Task LoadingObjects
loadObjects root details modules =
  Task.io $
  do  mvar <- Details.loadObjects root details
      mvars <- traverse (loadObject root) modules
      return $ LoadingObjects mvar (Map.fromList mvars)


loadObject :: FilePath -> Build.Module -> IO (ModuleName.Raw, MVar (Maybe Opt.LocalGraph))
loadObject root modul =
  case modul of
    Build.Fresh name _ graph ->
      do  mvar <- newMVar (Just graph)
          return (name, mvar)

    Build.Cached name _ _ ->
      do  mvar <- newEmptyMVar
          _ <- forkIO $ putMVar mvar =<< File.readBinary (Stuff.elmo root name)
          return (name, mvar)


data LoadedObjects =
  LoadedObjects
    { _foreign :: Opt.GlobalGraph
    , _locals :: Map.Map ModuleName.Raw Opt.LocalGraph
    }


finalizeObjects :: LoadingObjects -> Task LoadedObjects
finalizeObjects (LoadingObjects mvar mvars) =
  Task.eio id $
  do  result  <- readMVar mvar
      results <- traverse readMVar mvars
      case liftM2 LoadedObjects result (sequence results) of
        Just loaded -> return (Right loaded)
        Nothing     -> return (Left Exit.MakeCannotLoadArtifacts)



-- LOAD TYPES


loadTypes :: FilePath -> Map.Map ModuleName.Canonical I.DependencyInterface -> [Build.Module] -> Task Extract.Types
loadTypes root ifaces modules =
  Task.eio id $
  do  mvars <- traverse (loadTypesHelp root) modules
      let !foreigns = Extract.mergeMany (Map.elems (Map.mapWithKey Extract.fromDependencyInterface ifaces))
      results <- traverse readMVar mvars
      case sequence results of
        Just ts -> return (Right (Extract.merge foreigns (Extract.mergeMany ts)))
        Nothing -> return (Left Exit.MakeCannotLoadArtifacts)


loadTypesHelp :: FilePath -> Build.Module -> IO (MVar (Maybe Extract.Types))
loadTypesHelp root modul =
  case modul of
    Build.Fresh name iface _ ->
      newMVar (Just (Extract.fromInterface name iface))

    Build.Cached name _ ciMVar ->
      do  cachedInterface <- readMVar ciMVar
          case cachedInterface of
            Build.Unneeded ->
              do  mvar <- newEmptyMVar
                  _ <- forkIO $
                    do  maybeIface <- File.readBinary (Stuff.elmo root name)
                        putMVar mvar (Extract.fromInterface name <$> maybeIface)
                  return mvar

            Build.Loaded iface ->
              newMVar (Just (Extract.fromInterface name iface))

            Build.Corrupted ->
              newMVar Nothing



-- GATHER MAINS


gatherMains :: Pkg.Name -> LoadedObjects -> NE.List Build.Main -> Map.Map ModuleName.Canonical Opt.Main
gatherMains pkg (LoadedObjects _ locals) buildMains =
  Map.fromList $ Maybe.mapMaybe (lookupMain pkg locals) (NE.toList buildMains)


lookupMain :: Pkg.Name -> Map.Map ModuleName.Raw Opt.LocalGraph -> Build.Main -> Maybe (ModuleName.Canonical, Opt.Main)
lookupMain pkg locals buildMain =
  let
    toPair name (Opt.LocalGraph maybeMain _ _) =
      (,) (ModuleName.Canonical pkg name) <$> maybeMain
  in
  case buildMain of
    Build.Inside  name     -> toPair name =<< Map.lookup name locals
    Build.Outside name _ g -> toPair name g



-- PARSERS


reportType :: Parser ReportType
reportType =
  Parser
    { _singular = "report type"
    , _plural = "report types"
    , _parser = \string -> if string == "json" then Just Json else Nothing
    , _suggest = \_ -> return ["json"]
    , _examples = \_ -> return ["json"]
    }


output :: Parser Output
output =
  Parser
    { _singular = "output file"
    , _plural = "output files"
    , _parser = parseOutput
    , _suggest = \_ -> return []
    , _examples = \_ -> return [ "elm.js", "index.html", "/dev/null" ]
    }


parseOutput :: String -> Maybe Output
parseOutput name
  | isDevNull name      = Just DevNull
  | hasExt ".html" name = Just (Html name)
  | hasExt ".js"   name = Just (JS name)
  | otherwise           = Nothing


docsFile :: Parser FilePath
docsFile =
  Parser
    { _singular = "json file"
    , _plural = "json files"
    , _parser = \name -> if hasExt ".json" name then Just name else Nothing
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["docs.json","documentation.json"]
    }


hasExt :: String -> String -> Bool
hasExt ext path =
  FP.takeExtension path == ext && length path > length ext


isDevNull :: String -> Bool
isDevNull name =
  name == "/dev/null" || name == "NUL" || name == "$null"
