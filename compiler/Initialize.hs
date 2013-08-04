module Initialize (buildFromSource, getSortedDependencies) where

import Data.Data
import Control.Monad.State
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Exit
import System.FilePath as FP
import Text.PrettyPrint (Doc)

import SourceSyntax.Everything
import SourceSyntax.Type
import qualified Parse.Parse as Parse
import qualified Metadata.Prelude as Prelude
import qualified Transform.Check as Check
import qualified Transform.SortDefinitions as SD
import qualified Type.Inference as TI
import qualified Type.Constrain.Declaration as TcDecl
import qualified Transform.Canonicalize as Canonical

buildFromSource :: Bool -> Interfaces -> String -> Either [Doc] (MetadataModule () ())
buildFromSource noPrelude interfaces source =
  do let add = if noPrelude then id else Prelude.add
     modul@(Module _ _ _ decls') <- add `fmap` Parse.program source

     -- check for structural errors
     Module names exs ims decls <-
         case Check.mistakes decls' of
           [] -> return modul
           ms -> Left ms

     let exports'
             | null exs =
                 let get = Set.toList . SD.boundVars in
                 concat [ get pattern | Definition (Def pattern _) <- decls ] ++
                 concat [ map fst ctors | Datatype _ _ ctors <- decls ]
             | otherwise = exs

     metaModule <- Canonical.metadataModule interfaces $ MetadataModule {
           names = names,
           path = FP.joinPath names,
           exports = exports',
           imports = ims,
           -- reorder AST into strongly connected components
           program = SD.sortDefs . dummyLet $ TcDecl.toExpr decls,
           types = Map.empty,
           datatypes = [ (name,vars,ctors) | Datatype name vars ctors <- decls ],
           fixities = [ (assoc,level,op) | Fixity assoc level op <- decls ],
           aliases = [ (name,tvs,tipe) | TypeAlias name tvs tipe <- decls ],
           foreignImports = [ (evt,v,name,typ) | ImportEvent evt v name typ <- decls ],
           foreignExports = [ (evt,name,typ) | ExportEvent evt name typ <- decls ]
          }

     types <- TI.infer interfaces metaModule

     return $ metaModule { types = types }


getSortedDependencies :: Bool -> FilePath -> IO [String]
getSortedDependencies noPrelude root =
    sortDeps =<< readDeps noPrelude root

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

readDeps :: Bool -> FilePath -> IO [Deps]
readDeps noPrelude root = evalStateT (go root) Set.empty
  where
    builtIns = if noPrelude then Set.empty
                            else Set.fromList (Map.keys Prelude.interfaces)

    go :: FilePath -> StateT (Set.Set String) IO [Deps]
    go root = do
      txt <- liftIO $ readFile root
      case Parse.dependencies txt of
        Left err -> liftIO (putStrLn msg >> print err >> exitFailure)
            where msg = "Error resolving dependencies in " ++ root ++ ":"
                    
        Right (name,deps) ->
            do seen <- get
               let realDeps = Set.difference (Set.fromList deps) builtIns
                   newDeps = Set.difference (Set.filter (not . isNative) realDeps) seen
               put (Set.insert name (Set.union newDeps seen))
               rest <- mapM (go . toFilePath) (Set.toList newDeps)
               return ((makeRelative "." root, name, Set.toList realDeps) : concat rest)
                       

isNative name = List.isPrefixOf "Native." name

toFilePath :: String -> FilePath
toFilePath name = map swapDots name ++ ext
    where swapDots '.' = '/'
          swapDots  c  =  c
          ext = if isNative name then ".js" else ".elm"
