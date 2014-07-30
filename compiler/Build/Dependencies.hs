{-# OPTIONS_GHC -W #-}
module Build.Dependencies (Recipe(..), getBuildRecipe) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error
import qualified Control.Monad.State as State
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Directory
import System.FilePath as FP

import qualified AST.Module as Module
import AST.ProgramHeader
import qualified AST.ProgramHeader as ProgramHeader
import Build.SrcFile
import qualified Build.SrcFile as SrcFile
import qualified Elm.Internal.Paths as Path
import qualified Elm.Internal.Dependencies as Deps (withNative)
import qualified Elm.Internal.Libraries as L (withVersions)
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Version as V

data Recipe = Recipe
    { _elmFiles :: [ResolvedSrcFile]
    , _jsFiles  :: [FilePath]
    }

data Dir = SrcDir  FilePath
         | Package N.Name FilePath
         deriving Show

dirPath :: Dir -> FilePath
dirPath (SrcDir path)    = path
dirPath (Package _ path) = path

packageName :: Dir -> Maybe N.Name
packageName (Package n _) = Just n
packageName _             = Nothing

getBuildRecipe :: Bool -> [FilePath] -> Module.Interfaces -> UnresolvedSrcFile -> ErrorT String IO Recipe
getBuildRecipe isMake srcDirs builtIns srcFile =
  do directories <- getDependencies
     let allSrcDirs = (map SrcDir srcDirs) ++ (map (uncurry Package) directories)
     if not isMake
       then (\src -> Recipe [src] []) <$> resolveSrcFile allSrcDirs srcFile
       else do
       jsFiles  <- nativeFiles ("." : (map snd directories))
       nodes    <- collectDependencies allSrcDirs builtIns srcFile
       elmFiles <- sortElmFiles nodes
       return (Recipe elmFiles jsFiles)

-- | Based on the projects elm_dependencies.json, find all of the paths and
--   dependency information we might need.
getDependencies :: ErrorT String IO [(N.Name, FilePath)]
getDependencies =
    do exists <- liftIO $ doesFileExist Path.librariesFile
       if not exists then return [] else getPaths
    where
      getPaths :: ErrorT String IO [(N.Name, FilePath)]
      getPaths =
        L.withVersions Path.librariesFile $ \vers ->
            mapM getPath vers

      getPath :: (N.Name, V.Version) -> ErrorT String IO (N.Name, FilePath)
      getPath (name,version) = do
        let path = Path.dependencyDirectory </> N.toFilePath name </> show version
        exists <- liftIO $ doesDirectoryExist path
        if exists
          then return (name, path)
          else throwError (notFound name version)

      -- TODO: This message should be changed
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
        Deps.withNative (dir </> Path.dependencyFile) $ \native ->
            return (map (toPath dir) native)

    toPath dir moduleName =
        dir </> joinPath (split moduleName) <.> "js"
        
split :: String -> [String]
split moduleName = go [] moduleName
  where
    go paths str =
        case break (=='.') str of
          (path, _:rest) -> go (paths ++ [path]) rest
          (path, [])     -> paths ++ [path]

type DependencyNode = (ResolvedSrcFile, ModuleId, [ModuleId])

sortElmFiles :: [DependencyNode] -> ErrorT String IO [ResolvedSrcFile]
sortElmFiles depends = do
    if null mistakes
      then return (concat sccs)
      else throwError $ msg ++ unlines (map show mistakes)
  where
    sccs = map Graph.flattenSCC $ Graph.stronglyConnComp depends

    mistakes = filter (\scc -> length scc > 1) sccs
    msg = "A cyclical module dependency or was detected in:\n"

collectDependencies :: [Dir] -> Module.Interfaces -> UnresolvedSrcFile
                    -> ErrorT String IO [DependencyNode]
collectDependencies srcDirs rawBuiltIns srcFile =
    State.evalStateT (go srcFile) Set.empty
  where
    builtIns :: Set.Set (Maybe N.Name, ModuleName)
    builtIns = Set.fromList . map ((,) Nothing . split) $ Map.keys rawBuiltIns

    nativeOrBuiltIn = (||) <$> isNativeModule <*> isBuiltInModule
    isBuiltInModule = flip Map.member rawBuiltIns . ProgramHeader.toName

    go :: UnresolvedSrcFile -> State.StateT (Set.Set ModuleId) (ErrorT String IO) [DependencyNode]
    go srcFile = do
      let deps     = filter (not . nativeOrBuiltIn) . getDeps $ srcFile
          moduleId = SrcFile.moduleId srcFile
          validSrcDirs = case SrcFile.pkg srcFile of
            Nothing -> srcDirs
            n       -> filter ((==) n . packageName) srcDirs
      depSrcs <- lift $ forM deps (findSrcFile moduleId validSrcDirs)
      seen <- State.get

      let depIds    = Set.fromList . map (\(_,id,_) -> id) $ depSrcs
          newDeps   = Set.difference depIds seen
          isNew (_, key, _) = Set.member key newDeps
      State.put (Set.insert moduleId (Set.union newDeps seen))

      let resolved = SrcFile.resolve srcFile . map (\(_,ids,_) -> ids) $ depSrcs
      rest <- mapM (go . (\(x,_,_) -> x)) . filter isNew $ depSrcs
      return ((resolved, moduleId, Set.toList depIds) : concat rest)
    

resolveSrcFile :: [Dir] -> UnresolvedSrcFile -> ErrorT String IO ResolvedSrcFile
resolveSrcFile dirs oSrcFile =
  SrcFile.resolve oSrcFile . map (\(_,mid,_) -> mid) <$> forM deps (findSrcFile srcId dirs)
  where deps  = SrcFile.getDeps oSrcFile
        srcId = (Nothing, SrcFile.moduleName oSrcFile)
  

findSrcFile :: ModuleId -> [Dir] -> ModuleName -> ErrorT String IO (UnresolvedSrcFile, ModuleId, [ModuleId])
findSrcFile (parentPkg, parentName) dirs name =
    foldr tryDir notFound dirs
  where
    tryDir dir next = do
      let path' = dirPath dir </> path
      exists <- liftIO $ doesFileExist path'
      if not exists
        then next
        else do
        let pkg = packageName dir
        src <- SrcFile.load pkg path'
        let header  = SrcFile.header src
            imports = ProgramHeader._imports $ header
            withPkg = (,) pkg
        return (src, (pkg, ProgramHeader._names header), map (withPkg . fst) imports)

    parentModuleName' = "module '" ++ (ProgramHeader.toName $ parentName) ++ "'" ++ mayPkg
    mayPkg = case parentPkg of
      Nothing -> ""
      Just n  -> "in package " ++ show n

    path = ProgramHeader.modulePath name <.> "elm"
    notFound = throwError $ unlines
        [ "When finding the imports declared in " ++ parentModuleName' ++ ", could not find file: " ++ path
        , "    If you created this module, but it is in a subdirectory that does not"
        , "    exactly match the module name, you may need to use the --src-dir flag."
        , ""
        , "    If it is part of a 3rd party library, it needs to be declared"
        , "    as a dependency in your project's " ++ Path.dependencyFile ++ " file."
        ]

isNativeModule :: [String] -> Bool
isNativeModule ("Native":_) = True
isNativeModule _            = False
