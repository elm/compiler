{-# OPTIONS_GHC -W #-}
module Build.Dependencies (Recipe(..), getBuildRecipe) where

import Control.Applicative ((<$>))
import Control.Monad.Error
import qualified Control.Monad.State as State
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import System.Directory
import System.FilePath as FP

import Build.Metadata (Root)
import qualified Build.Metadata as Metadata
import qualified AST.Module as Module
import qualified Elm.Internal.Assets as Asset
import qualified Elm.Internal.Dependencies as Deps (withNative)
import qualified Elm.Internal.SolvedDependencies as SD
import qualified Elm.Internal.Name as N
import qualified Elm.Internal.Version as V
import Parse.Helpers (iParse)
import Parse.Parse (programHeader)

---- RECIPES ----

data Recipe = Recipe
    { _elmFiles :: [Metadata.WithDeps]
    , _jsFiles :: [FilePath]
    }

getBuildRecipe :: Bool -> [FilePath] -> Module.Interfaces -> FilePath
               -> ErrorT String IO Recipe
getBuildRecipe isMake srcDirs interfaces filePath =
  do packages <- getPackages
     let roots = map Metadata.SrcDir srcDirs ++ map (uncurry Metadata.Package) packages
     case isMake of
       False ->
           do header <- readModuleHeader filePath
              let metadata =
                      Metadata.Metadata filePath (Module._names header) Nothing []
              return (Recipe [metadata] [])

       True ->
           do jsFiles <- nativeFiles ("." : (map snd packages))
              pathNodes <- transitiveDependencies roots interfaces filePath
              elmFiles <- sort pathNodes
              return (Recipe elmFiles jsFiles)

{-| Read the solved dependencies to find all possible Roots
-}
getPackages :: ErrorT String IO [(N.Name, FilePath)]
getPackages =
    do exists <- liftIO $ doesFileExist Asset.solvedDependencies
       if not exists then return [] else getPaths
    where
      getPaths :: ErrorT String IO [(N.Name, FilePath)]
      getPaths =
        SD.getAnd Asset.solvedDependencies $ \vers ->
            mapM getPath vers

      getPath :: (N.Name, V.Version) -> ErrorT String IO (N.Name, FilePath)
      getPath (name,version) = do
        let path = Asset.packagesDirectory </> N.toFilePath name </> show version
        exists <- liftIO $ doesDirectoryExist path
        if exists
          then return (name, path)
          else throwError (notFound name version)

      notFound :: N.Name -> V.Version -> String
      notFound name version =
          unlines
          [ "Error: the " ++ Asset.packagesDirectory ++ " directory may be corrupted."
          , "    The " ++ Asset.solvedDependencies ++ " file says you depend on package"
          , "    " ++ show name ++ " " ++ show version ++ " but it was not found."
          , ""
          , "Rebuild the " ++ Asset.packagesDirectory ++ " by deleting the directory and running:"
          , ""
          , "    elm-get install"
          ]

nativeFiles :: [FilePath] -> ErrorT String IO [FilePath]
nativeFiles directories =
  do exists <- liftIO $ doesFileExist Asset.dependencyFile
     if not exists
       then return []
       else concat <$> mapM getNativeFiles directories
  where
    getNativeFiles dir =
        Deps.withNative (dir </> Asset.dependencyFile) $ \native ->
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
--}

---- DEPENDENCIES ----

type Loc = Metadata.Located

type MetadataNode = (Metadata.WithDeps, Loc, [Loc])

sort :: [MetadataNode] -> ErrorT String IO [Metadata.WithDeps]
sort nodes =
    if null mistakes
      then return (concat sccs)
      else throwError $ msg ++ unlines (map show mistakes)
  where
    sccs = map Graph.flattenSCC (Graph.stronglyConnComp nodes)

    mistakes = filter (\scc -> length scc > 1) sccs
    msg = "A cyclical module dependency was detected in:\n"


-- TRANSITIVE DEPENDENCIES

{-| Start with a given path and find all of the transitive dependencies. Results
are split up into Native files and normal files. Neither are sorted in any
particular order.
-}
transitiveDependencies :: [Root] -> Module.Interfaces -> FilePath
                       -> ErrorT String IO [MetadataNode]
transitiveDependencies roots interfaces filePath =
    State.evalStateT (go filePath Nothing) Set.empty
  where
    go :: FilePath -> Maybe N.Name
       -> State.StateT (Set.Set Loc) (ErrorT String IO) [MetadataNode]
    go filePath package =
      do header <- lift $ readModuleHeader filePath

         let location = Metadata.Metadata filePath (Module._names header) package ()

         let dependencyNames = map fst (Module._imports header)

         let (thirdParty, _native, _builtIn) =
                 categorizeModules interfaces dependencyNames

         thirdPartyLocations <-
             lift $ mapM (locateModule location roots) thirdParty

         -- Determine which paths are new for future exploration
         -- Then mark all dependencies as visited so that we don't traverse them
         -- again while exploring the new paths.
         newLocations <- freshLocations thirdPartyLocations
         markVisited (location : thirdPartyLocations)

         -- create fully fleshed out metadata
         let metadata = Metadata.addDeps thirdPartyLocations location

         let node = (metadata, location, thirdPartyLocations)

         dependencyNodes <-
             forM newLocations $ \location ->
                 go (Metadata._path location) (Metadata._pkg location)

         return (node : concat dependencyNodes)

{-| Read the module header from a particular file. -}
readModuleHeader :: FilePath -> ErrorT String IO Module.Header
readModuleHeader filePath =
  do txt <- liftIO $ readFile filePath
     case iParse programHeader txt of
       Left err ->
           throwError ("Error parsing file " ++ filePath ++ ":\n" ++ show err)

       Right header ->
           return header

{-| Based on a list of third party dependencies, figure out which paths still
need to be explored.
-}
freshLocations :: (Monad m) => [Loc]
               -> State.StateT (Set.Set Loc) m [Loc]
freshLocations locations =
  do visited <- State.get
     let fresh = Set.difference (Set.fromList locations) visited
     return (Set.toList fresh)

{-| Mark a set of paths as visited. -}
markVisited :: (Monad m) => [Loc] -> State.StateT (Set.Set Loc) m ()
markVisited paths =
    State.modify (Set.union (Set.fromList paths))

{-| Categorize modules to make them easy to locate. -}
categorizeModules :: Module.Interfaces -> [Module.Name]
                  -> ([Module.Name], [Module.Name], [Module.Name])
categorizeModules interfaces moduleNames =
    (thirdParty, native, builtIn)
  where
    (native , rest      ) = List.partition isNative moduleNames
    (builtIn, thirdParty) = List.partition isBuiltIn rest

    isNative :: Module.Name -> Bool
    isNative name =
        case name of
          "Native":_ -> True
          _          -> False

    isBuiltIn :: Module.Name -> Bool
    isBuiltIn name =
        Map.member (Module.nameToString name) interfaces

locateModule :: Loc -> [Root] -> Module.Name -> ErrorT String IO Loc
locateModule parentLocation roots moduleName =
  do possibleRoots <- Maybe.catMaybes <$> mapM tryRoot roots
     case possibleRoots of
       [] -> throwError notFound

       [root] -> found root

       -- If the parent module is in a package, we should resolve to the
       -- module in the same package if possible.
       _ ->
           case Metadata._pkg parentLocation of
             Nothing -> throwError (tooMany possibleRoots)
             name ->
                 case List.find ((==) name . Metadata.packageName) possibleRoots of
                   Nothing -> throwError (tooMany possibleRoots)
                   Just root -> found root
  where
    filePath = foldr1 (</>) moduleName <.> "elm"

    tryRoot root =
      do let fullPath = Metadata.rootDirectory root </> filePath
         exists <- liftIO $ doesFileExist fullPath
         if exists
           then return (Just root)
           else return Nothing

    found root =
        let fullPath = Metadata.rootDirectory root </> filePath
            pkg = Metadata.packageName root
        in  return $ Metadata.Metadata fullPath moduleName pkg ()


    parentModuleName =
        Module.nameToString (Metadata._name parentLocation)

    indent str = "    " ++ str ++ "\n"

    report msg =
        "Error: " ++ msg ++ " '" ++ Module.nameToString moduleName ++
        "'\nIt is imported by module '" ++ parentModuleName ++ "'"

    notFound =
        unlines
        [ report "could not find module"
        , "We searched in the following locations:"
        , listLocations roots
        , "Potential problems could be:"
        , "  * Misspelled the module name"
        , "  * Need a --src-dir flag to explore additional directories"
        , "  * Need to declare a new dependency in " ++ Asset.dependencyFile
        ]

    tooMany roots =
        report "found multiple modules named" ++
        "\nModules with that name were found in the following locations:\n" ++
        listLocations roots

    listLocations roots =
        listSrcDirs roots ++ listPackages roots

    listSrcDirs roots =
        case Metadata.srcDirs roots of
          [] -> ""
          [srcDir] -> "\nSource directory:\n" ++ indent srcDir
          srcDirs -> "\nSource directories:\n" ++ concatMap indent srcDirs

    listPackages roots =
        case Metadata.packages roots of
          [] -> ""
          [pkg] -> "\nPackage:\n" ++ indent (show pkg)
          pkgs -> "\nPackages:\n" ++ concatMap (indent . show) pkgs
