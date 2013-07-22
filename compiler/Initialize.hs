module Initialize (buildFromSource, getSortedModuleNames, Interfaces) where

import qualified Data.Map as Map
import Data.Data
import qualified Data.Graph as Graph
import qualified Data.List as List
import System.Exit
import System.FilePath as FP
import Text.PrettyPrint (Doc)

import SourceSyntax.Everything
import SourceSyntax.Type
import qualified Parse.Parse as Parse
import qualified Metadata.Libraries as Libs
import qualified Transform.Check as Check
import qualified Transform.SortDefinitions as SD
import qualified Type.Inference as TI
import qualified Type.Constrain.Declaration as TcDecl

import System.IO.Unsafe
import SourceSyntax.PrettyPrint

buildFromSource :: Interfaces -> String -> Either [Doc] (MetadataModule () ())
buildFromSource interfaces source =
  do modul@(Module _ _ _ decls') <- Parse.program source

     -- check for structural errors
     Module names exs ims decls <-
         case Check.mistakes decls' of
           [] -> return modul
           ms -> Left ms
     
     let metaModule = MetadataModule {
           names = names,
           path = FP.joinPath names,
           exports = exs,
           imports = ims,
           -- reorder AST into strongly connected components
           program = SD.sortDefs . dummyLet $ TcDecl.toExpr decls,
           types = Map.empty,
           datatypes = [ (name, vars, ctors) | Datatype name vars ctors <- decls ],
           fixities = [ (assoc,level,op) | Fixity assoc level op <- decls ],
           aliases = [ (name,tvs,tipe) | TypeAlias name tvs tipe <- decls ],
           foreignImports = [ (evt,v,name,typ) | ImportEvent evt v name typ <- decls ],
           foreignExports = [ (evt,name,typ) | ExportEvent evt name typ <- decls ]
          }

     types <- TI.infer interfaces metaModule

     return $ metaModule { types = types }


getSortedModuleNames :: FilePath -> IO [String]
getSortedModuleNames root =
    sortDeps =<< readDeps [] root

type Deps = (String, [String])

sortDeps :: [Deps] -> IO [String]
sortDeps depends =
    if null mistakes
    then return (concat sccs)
    else print msg >> mapM print mistakes >> exitFailure
  where
    graph = map (\(name, deps) -> (toFilePath name, name, deps)) depends
    sccs = map Graph.flattenSCC $ Graph.stronglyConnComp graph

    mistakes = filter (\scc -> length scc > 1) sccs
    msg = "A cyclical module dependency or was detected in: "

readDeps :: [FilePath] -> FilePath -> IO [Deps]
readDeps seen root = do
  txt <- readFile root
  case Parse.dependencies txt of
    Left err -> putStrLn msg >> print err >> exitFailure
        where msg = "Error resolving dependencies in " ++ root ++ ":"
                    
    Right (name,deps) ->
        do rest <- mapM (readDeps seen' . toFilePath) newDeps
           return ((name, realDeps) : concat rest)
        where
          realDeps = filter (`notElem` builtIns) deps
          newDeps = filter (\d -> not (d `elem` seen || isNative d)) realDeps
          seen' = root : seen ++ newDeps
          builtIns = Map.keys Libs.libraries
                       

isNative name = List.isPrefixOf "Native." name

toFilePath :: String -> FilePath
toFilePath name = map swapDots name ++ ext
    where swapDots '.' = '/'
          swapDots  c  =  c
          ext = if isNative name then ".js" else ".elm"
