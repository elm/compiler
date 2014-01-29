{-# OPTIONS_GHC -W #-}
module Build.Dependencies (getSortedDependencies, readDeps) where

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
import System.FilePath as FP

import Build.Print (failure)

import qualified SourceSyntax.Module as Module
import qualified Parse.Parse as Parse
import qualified Elm.Internal.Paths as Path
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Dependencies as Deps

getSortedDependencies :: [FilePath] -> Module.Interfaces -> FilePath -> IO [String]
getSortedDependencies srcDirs builtIns root =
    do extras <- extraDependencies
       let allSrcDirs = srcDirs ++ Maybe.fromMaybe [] extras
       result <- runErrorT $ readAllDeps allSrcDirs builtIns root
       case result of
         Right deps -> sortDeps deps
         Left err -> failure err

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

readAllDeps :: [FilePath] -> Module.Interfaces -> FilePath -> ErrorT String IO [Deps]
readAllDeps srcDirs rawBuiltIns filePath =
    State.evalStateT (go Nothing filePath) Set.empty
  where
    builtIns :: Set.Set String
    builtIns = Set.fromList $ Map.keys rawBuiltIns

    go :: Maybe String -> FilePath -> State.StateT (Set.Set String) (ErrorT String IO) [Deps]
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
    Left err -> throwError $ msg ++ show err
      where msg = "Error resolving dependencies in " ++ path ++ ":\n"
    Right o  -> return o

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
