module Build.Dependencies (getSortedDependencies) where

import Data.Data
import Control.Applicative
import Control.Monad.State
import qualified Data.Char as Char
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import System.Directory
import System.Exit
import System.FilePath as FP
import System.IO
import Text.PrettyPrint (Doc)

import SourceSyntax.Everything
import qualified SourceSyntax.Type as Type
import qualified Parse.Parse as Parse
import qualified Metadata.Prelude as Prelude
import qualified Transform.Check as Check
import qualified Transform.SortDefinitions as SD
import qualified Type.Inference as TI
import qualified Type.Constrain.Declaration as TcDecl
import qualified Transform.Canonicalize as Canonical
import qualified Elm.Internal.Version as V

getSortedDependencies :: [FilePath] -> Interfaces -> FilePath -> IO [String]
getSortedDependencies srcDirs builtIns root =
    do extras <- additionalSourceDirs
       sortDeps =<< readDeps (srcDirs ++ extras) builtIns root

additionalSourceDirs :: IO [FilePath]
additionalSourceDirs =
    do exists <- doesDirectoryExist deps
       if exists then getProjects deps else return []
    where
      deps = "elm_dependencies"

      getProjects dir = do
        subs <- map (dir </>) <$> getDirectoryContents dir
        projects <- filterM isProject subs
        Maybe.catMaybes <$> mapM getLatest projects

      getLatest project = do
        subs <- getDirectoryContents project
        case Maybe.mapMaybe V.fromString subs of
          [] -> return Nothing
          versions -> return $ Just $ (project </>) $ show maxVersion
              where maxVersion = List.maximum versions

      isProject path = do
        exists <- doesDirectoryExist path
        return $ exists && path `notElem` [".","..","_internals"]

type Deps = (FilePath, String, [String])

sortDeps :: [Deps] -> IO [String]
sortDeps depends =
    if null mistakes
    then return (concat sccs)
    else print msg >> mapM print mistakes >> exitFailure
  where
    sccs = map Graph.flattenSCC $ Graph.stronglyConnComp depends

    mistakes = filter (\scc -> length scc > 1) sccs
    msg = "A cyclical module dependency or was detected in: "

readDeps :: [FilePath] -> Interfaces -> FilePath -> IO [Deps]
readDeps srcDirs builtIns root = do
  let ifaces = (Set.fromList . Map.keys) builtIns
  evalStateT (go ifaces root) Set.empty
  where
    go :: Set.Set String -> FilePath -> StateT (Set.Set String) IO [Deps]
    go builtIns root = do
      (root', txt) <- liftIO $ getFile srcDirs root
      case Parse.dependencies txt of
        Left err -> liftIO (putStrLn msg >> print err >> exitFailure)
            where msg = "Error resolving dependencies in " ++ root' ++ ":"

        Right (name,deps) ->
            do seen <- get
               let realDeps = Set.difference (Set.fromList deps) builtIns
                   newDeps = Set.difference (Set.filter (not . isNative) realDeps) seen
               put (Set.insert name (Set.union newDeps seen))
               rest <- mapM (go builtIns . toFilePath) (Set.toList newDeps)
               return ((makeRelative "." root', name, Set.toList realDeps) : concat rest)

getFile :: [FilePath] -> FilePath -> IO (FilePath,String)
getFile [] path = do
  hPutStrLn stderr $ unlines
    [ "Could not find file: " ++ path
    , "    If it is not in the root directory of your project, use"
    , "    --src-dir to declare additional locations for source files." ]
  exitFailure

getFile (dir:dirs) path = do
  let path' = dir </> path
  exists <- doesFileExist path'
  case exists of
    True -> (,) path' `fmap` readFile path'
    False -> getFile dirs path

isNative name = List.isPrefixOf "Native." name

toFilePath :: String -> FilePath
toFilePath name = map swapDots name ++ ext
    where swapDots '.' = '/'
          swapDots  c  =  c
          ext = if isNative name then ".js" else ".elm"
