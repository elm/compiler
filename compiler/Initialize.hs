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
import qualified Transform.Optimize as Optimize
import qualified Transform.Check as Check
import System.Exit
import System.FilePath


unify = undefined
hints = undefined


checkMistakes :: (Data t, Data v) => Module t v -> Either String (Module t v)
checkMistakes modul@(Module name ex im decls) = 
  case Check.mistakes decls of
    [] -> return modul
    ms -> Left (unlines ms)

checkTypes :: (Data t, Data v) => Module t v -> Either String (Module t v)
checkTypes modul =
  do subs <- unify hints modul
     subs `seq` return (Optimize.optimize (renameModule modul))

check :: (Data t, Data v) => Module t v -> Either String (Module t v)
check = checkMistakes >=> checkTypes

buildFromSource :: (Data t, Data v) => Bool -> String -> Either String (Module t v)
buildFromSource noPrelude src =
    let add = if noPrelude then id else Libs.addPrelude in
    (check . add) =<< parseProgram src

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
