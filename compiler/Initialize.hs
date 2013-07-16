module Initialize (buildFromSource, getSortedModuleNames) where

import Control.Applicative ((<$>))
import Control.Monad.Error
import Data.List (nub)
import qualified Data.Map as Map
import Data.Data
import Data.List (intercalate,partition)
import System.Exit
import System.FilePath as FP
import Text.PrettyPrint (Doc)

import SourceSyntax.Everything
import SourceSyntax.Declaration (Assoc)
import qualified SourceSyntax.Location as Loc
import qualified Parse.Parse as Parse
import qualified Metadata.Libraries as Libs
import qualified Transform.Optimize as Optimize
import qualified Transform.Check as Check
import qualified Transform.SortDefinitions as SD
import qualified Type.Inference as TI
import qualified Type.Constrain.Declaration as TcDecl
import qualified Type.Type as T


buildFromSource :: (Data t, Data v) => Bool -> String -> Either [Doc] (MetadataModule t v)
buildFromSource noPrelude src =
  do modul <- Parse.program src

     -- check for structural errors
     Module names exs ims decls <- checkMistakes modul

     -- reorder AST into strongly connected components
     let metaModule = MetadataModule {
           names = names,
           path = FP.joinPath names,
           exports = exs,
           imports = ims,
           defs = fst . SD.flattenLets [] . SD.sortDefs . dummyLet $ TcDecl.toExpr decls,
           types = Map.empty,
           fixities = [ (assoc,level,op) | Fixity assoc level op <- decls ],
           aliases = [ (name,tvs,tipe) | TypeAlias name tvs tipe <- decls ],
           foreignImports = [ (evt,v,name,typ) | ImportEvent evt v name typ <- decls ],
           foreignExports = [ (evt,name,typ) | ExportEvent evt name typ <- decls ]
          }

     types <- TI.infer metaModule
     return $ metaModule { types = types }

  where
    checkMistakes modul@(Module _ _ _ decls) = 
        case Check.mistakes decls of
          [] -> return modul
          ms -> Left ms


getSortedModuleNames :: FilePath -> IO [String]
getSortedModuleNames root =
    sortDeps =<< readDeps [] root

type Deps = (String, [String])

sortDeps :: [Deps] -> IO [String]
sortDeps deps = go [] (nub deps)
    where
      msg = "A cyclical or missing module dependency or was detected in: "

      go :: [String] -> [Deps] -> IO [String]
      go sorted [] = return (map toFilePath sorted)
      go sorted unsorted =
          case partition (all (`elem` sorted) . snd) unsorted of
            ([],m:ms) -> do putStrLn (msg ++ intercalate ", " (map fst (m:ms)) ++ show sorted ++ show unsorted)
                            exitFailure
            (srtd,unsrtd) -> go (sorted ++ map fst srtd) unsrtd

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
          newDeps = filter (\d -> d `notElem` seen && not (isNative d)) realDeps
          seen' = root : seen ++ newDeps
          builtIns = Map.keys Libs.libraries
                       

isNative name = takeWhile (/='.') name == "Native"

toFilePath :: String -> FilePath
toFilePath name = map swapDots name ++ ext
    where swapDots '.' = '/'
          swapDots  c  =  c
          ext = if isNative name then ".js" else ".elm"
