{-# OPTIONS_GHC -W #-}
module Build.Dependencies (getSortedDependencies) where

import Control.Applicative
import Control.Monad.Error
import qualified Control.Monad.State as State
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import System.Directory
import System.Exit
import System.FilePath as FP
import System.IO

import qualified SourceSyntax.Module as Module
import qualified Parse.Parse as Parse
import qualified Elm.Internal.Paths as Path
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Dependencies as Deps

getSortedDependencies :: [FilePath] -> Module.Interfaces -> FilePath -> IO [String]
getSortedDependencies srcDirs builtIns root =
    do extras <- extraDependencies
       let allSrcDirs = srcDirs ++ Maybe.fromMaybe [] extras
       result <- runErrorT $ readDeps allSrcDirs builtIns root
       case result of
         Right deps -> sortDeps deps
         Left err -> failure $ err ++ if Maybe.isJust extras then "" else msg
             where msg = "\nYou may need to create a " ++
                         Path.dependencyFile ++
                         " file if you\nare trying to use a 3rd party library."

failure msg = hPutStrLn stderr msg >> exitFailure

extraDependencies :: IO (Maybe [FilePath])
extraDependencies =
    do exists <- doesFileExist Path.dependencyFile
       if not exists then return Nothing else Just <$> getPaths
    where
      getPaths = do
        raw <- BSC.readFile Path.dependencyFile
        case Json.eitherDecode raw of
          Right (Deps.Mini deps) -> mapM validate deps
          Left err ->
              failure $ "Error reading the " ++ Path.dependencyFile ++ " file:\n" ++ err

      validate (name,version) = do
        let path = Path.dependencyDirectory </> toPath name version
        exists <- doesDirectoryExist path
        if exists then return path else failure (notFound name version)

      toPath name version = N.toFilePath name </> show version

      notFound name version =
          unlines
          [ "Your " ++ Path.dependencyFile ++ " file says you depend on library"
          , show name ++ " " ++ show version ++ " but it was not found."
          , "You may need to install it with:"
          , ""
          , "    elm-get install " ++ show name ++ " " ++ show version ]

type Deps = (FilePath, String, [String])

sortDeps :: [Deps] -> IO [String]
sortDeps depends =
    if null mistakes
      then return (concat sccs)
      else failure $ msg ++ unlines (map show mistakes)
  where
    sccs = map Graph.flattenSCC $ Graph.stronglyConnComp depends

    mistakes = filter (\scc -> length scc > 1) sccs
    msg = "A cyclical module dependency or was detected in:\n"

readDeps :: [FilePath] -> Module.Interfaces -> FilePath -> ErrorT String IO [Deps]
readDeps srcDirs builtIns root = do
  let ifaces = (Set.fromList . Map.keys) builtIns
  State.evalStateT (go ifaces root) Set.empty
  where
    go :: Set.Set String -> FilePath -> State.StateT (Set.Set String) (ErrorT String IO) [Deps]
    go builtIns root = do
      (root', txt) <- lift $ getFile srcDirs root
      case Parse.dependencies txt of
        Left err -> throwError $ msg ++ show err
            where msg = "Error resolving dependencies in " ++ root' ++ ":\n"

        Right (name,deps) ->
            do seen <- State.get
               let realDeps = Set.difference (Set.fromList deps) builtIns
                   newDeps = Set.difference (Set.filter (not . isNative) realDeps) seen
               State.put (Set.insert name (Set.union newDeps seen))
               rest <- mapM (go builtIns . toFilePath) (Set.toList newDeps)
               return ((makeRelative "." root', name, Set.toList realDeps) : concat rest)

getFile :: [FilePath] -> FilePath -> ErrorT String IO (FilePath,String)
getFile [] path =
    throwError $ unlines
    [ "Could not find file: " ++ path
    , "    If it is not in the root directory of your project, use"
    , "    --src-dir to declare additional locations for source files."
    , "    If it is part of a 3rd party library, it needs to be declared"
    , "    as a dependency in the " ++ Path.dependencyFile ++ " file." ]

getFile (dir:dirs) path = do
  let path' = dir </> path
  exists <- liftIO $ doesFileExist path'
  case exists of
    True  -> (,) path' `fmap` liftIO (readFile path')
    False -> getFile dirs path

isNative name = List.isPrefixOf "Native." name

toFilePath :: String -> FilePath
toFilePath name = map swapDots name ++ ext
    where swapDots '.' = '/'
          swapDots  c  =  c
          ext = if isNative name then ".js" else ".elm"
