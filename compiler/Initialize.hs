module Initialize (buildFromSource, getSortedModuleNames) where

import Control.Applicative ((<$>))
import Control.Monad.Error
import Data.List (lookup,nub)
import qualified Data.Map as Map
import Data.Data
import SourceSyntax.Everything
import Data.List (intercalate,partition)
import Parse.Parser (parseProgram, parseDependencies)
import qualified Metadata.Libraries as Libs
import Types.Types ((-:))
import Types.Hints (hints)
import Types.Unify (unify)
import Types.Alias (dealias, mistakes)
import Transform.Optimize
import System.Exit
import System.FilePath

checkMistakes :: (Data t, Data v) => Module t v -> Either String (Module t v)
checkMistakes modul@(Module name ex im stmts) = 
  case mistakes stmts of
    m:ms -> Left (unlines (m:ms))
    []   -> return modul

checkTypes :: (Data t, Data v) => Module t v -> Either String (Module t v)
checkTypes modul =
  do subs <- unify hints modul
     subs `seq` return (optimize (renameModule modul))

check :: (Data t, Data v) => Module t v -> Either String (Module t v)
check = checkMistakes >=> checkTypes

buildFromSource :: (Data t, Data v) => Bool -> String -> Either String (Module t v)
buildFromSource noPrelude src =
    let add = if noPrelude then id else Libs.addPrelude in
    (check . add) =<< (parseProgram src)

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
  case parseDependencies txt of
    Left err ->
        let msg = "Error resolving dependencies in " ++ root ++ ":\n" in
        putStrLn (msg ++ err) >> exitFailure
    Right (name,deps) ->
        do rest <-
             (mapM (readDeps seen' . toFilePath) newDeps) 
           let restWithNatives =
                 (map (\name -> (name,[])) nativeDeps):rest
           return ((name, realDeps) : concat restWithNatives)
        where
          realDeps = filter (`notElem` builtIns) deps
          newDeps = filter (\d -> d `notElem` seen && not (isNative d)) realDeps
          nativeDeps = filter (\d -> d `notElem` seen && isNative d) realDeps
          seen' = root : seen ++ newDeps ++ nativeDeps
          builtIns = Map.keys Libs.libraries
                       

isNative name = takeWhile (/='.') name == "Native"

toFilePath :: String -> FilePath
toFilePath name = map swapDots name ++ ext
    where swapDots '.' = '/'
          swapDots  c  =  c
          ext = if isNative name then ".js" else ".elm"
