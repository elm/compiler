{-# OPTIONS_GHC -W #-}
module Build.Dependencies (Recipe(..), getBuildRecipe, readDeps) where

import Control.Monad.Error
import qualified Control.Monad.State as State
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Directory
import System.FilePath as FP

import qualified AST.Module as Module
import qualified Parse.Parse as Parse
import qualified Elm.Internal.Paths as Path
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Version as V
import qualified Elm.Internal.Dependencies as Deps

data Recipe = Recipe
    { _elmFiles :: [FilePath]
    , _jsFiles :: [FilePath]
    }

getBuildRecipe :: [FilePath] -> Module.Interfaces -> FilePath -> ErrorT String IO Recipe
getBuildRecipe srcDirs builtIns root =
  do directories <- getDependencies
     jsFiles <- nativeFiles ("." : directories)
     let allSrcDirs = srcDirs ++ directories
     nodes <- collectDependencies allSrcDirs builtIns root
     elmFiles <- sortElmFiles nodes
     return (Recipe elmFiles jsFiles)

-- | Based on the projects elm_dependencies.json, find all of the paths and
--   dependency information we might need.
getDependencies :: ErrorT String IO [FilePath]
getDependencies =
    do exists <- liftIO $ doesFileExist Path.dependencyFile
       if not exists then return [] else getPaths
    where
      getPaths :: ErrorT String IO [FilePath]
      getPaths =
        Deps.withDeps Path.dependencyFile $ \deps ->
            mapM getPath (Deps.dependencies deps)

      getPath :: (N.Name, V.Version) -> ErrorT String IO FilePath
      getPath (name,version) = do
        let path = Path.dependencyDirectory </> N.toFilePath name </> show version
        exists <- liftIO $ doesDirectoryExist path
        if exists
          then return path
          else throwError (notFound name version)

      notFound :: N.Name -> V.Version -> String
      notFound name version =
          unlines
          [ "Your " ++ Path.dependencyFile ++ " file says you depend on library"
          , show name ++ " " ++ show version ++ " but it was not found."
          , "You may need to install it with:"
          , ""
          , "    elm-get install " ++ show name ++ " " ++ show version ]

nativeFiles :: [FilePath] -> ErrorT String IO [FilePath]
nativeFiles directories =
  do exists <- liftIO $ doesFileExist Path.dependencyFile
     if not exists
       then return []
       else concat `fmap` mapM getNativeFiles directories
  where
    getNativeFiles dir =
        Deps.withDeps (dir </> Path.dependencyFile) $ \deps ->
            return (map (toPath dir) (Deps.native deps))

    toPath dir moduleName =
        dir </> joinPath (split moduleName) <.> "js"
        
split :: String -> [String]
split moduleName = go [] moduleName
  where
    go paths str =
        case break (=='.') str of
          (path, _:rest) -> go (paths ++ [path]) rest
          (path, [])     -> paths ++ [path]

type DependencyNode = (FilePath, String, [String])

sortElmFiles :: [DependencyNode] -> ErrorT String IO [FilePath]
sortElmFiles depends =
    if null mistakes
      then return (concat sccs)
      else throwError $ msg ++ unlines (map show mistakes)
  where
    sccs = map Graph.flattenSCC $ Graph.stronglyConnComp depends

    mistakes = filter (\scc -> length scc > 1) sccs
    msg = "A cyclical module dependency or was detected in:\n"

collectDependencies :: [FilePath] -> Module.Interfaces -> FilePath
                    -> ErrorT String IO [DependencyNode]
collectDependencies srcDirs rawBuiltIns filePath =
    State.evalStateT (go Nothing filePath) Set.empty
  where
    builtIns :: Set.Set String
    builtIns = Set.fromList $ Map.keys rawBuiltIns

    go :: Maybe String -> FilePath -> State.StateT (Set.Set String) (ErrorT String IO) [DependencyNode]
    go parentModuleName filePath = do
      filePath' <- lift $ findSrcFile parentModuleName srcDirs filePath
      (moduleName, deps) <- lift $ readDeps filePath'
      seen <- State.get
      let realDeps = Set.difference (Set.fromList deps) builtIns
          newDeps = Set.difference (Set.filter (not . isNative) realDeps) seen
      State.put (Set.insert moduleName (Set.union newDeps seen))
      rest <- mapM (go (Just moduleName) . toFilePath) (Set.toList newDeps)
      return ((makeRelative "." filePath', moduleName, Set.toList realDeps) : concat rest)

readDeps :: FilePath -> ErrorT String IO (String, [String])
readDeps path = do
  txt <- lift $ readFile path
  case Parse.dependencies txt of
    Right o  -> return o
    Left err -> throwError $ msg ++ show err
      where msg = "Error resolving dependencies in " ++ path ++ ":\n"

findSrcFile :: Maybe String -> [FilePath] -> FilePath -> ErrorT String IO FilePath
findSrcFile parentModuleName dirs path =
    foldr tryDir notFound dirs
  where
    tryDir dir next = do
      let path' = dir </> path
      exists <- liftIO $ doesFileExist path'
      if exists
        then return path'
        else next

    parentModuleName' =
        case parentModuleName of
          Just name -> "module '" ++ name ++ "'"
          Nothing -> "the main module"

    notFound = throwError $ unlines
        [ "When finding the imports declared in " ++ parentModuleName' ++ ", could not find file: " ++ path
        , "    If you created this module, but it is in a subdirectory that does not"
        , "    exactly match the module name, you may need to use the --src-dir flag."
        , ""
        , "    If it is part of a 3rd party library, it needs to be declared"
        , "    as a dependency in your project's " ++ Path.dependencyFile ++ " file."
        ]

isNative :: String -> Bool
isNative name = List.isPrefixOf "Native." name

toFilePath :: String -> FilePath
toFilePath name = map swapDots name ++ ext
  where
    swapDots '.' = '/'
    swapDots  c  =  c
    ext = if isNative name then ".js" else ".elm"
