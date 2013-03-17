module Initialize (build, buildFromSource) where

import Control.Applicative ((<$>))
import Control.Monad.Error
import Data.List (lookup,nub)

import Ast
import Data.Either (lefts,rights)
import Data.List (intercalate,partition)
import Parse.Parser (parseProgram, preParse)
import Rename
import Types.Types ((-:))
import Types.Hints (hints)
import Types.Unify
import Types.Alias
import Optimize
import CompileToJS (jsModule)

checkMistakes :: Module -> Either String Module
checkMistakes modul@(Module name ex im stmts) = 
  case mistakes stmts of
    m:ms -> Left (unlines (m:ms))
    []   -> return modul

checkTypes :: Module -> Either String Module
checkTypes (Module name ex im stmts) =
    let stmts' = dealias stmts
        modul = Module name ex im stmts'
    in do subs <- unify hints modul
          let --im' | any ((=="Prelude") . fst) im = im
              --    | otherwise = ("Prelude", Importing []) : im
              modul' = optimize . renameModule $ Module name ex im stmts'
          subs `seq` return modul'

check :: Module -> Either String Module
check = checkMistakes >=> checkTypes

buildFromSource :: String -> Either String Module
buildFromSource src = check =<< parseProgram src

build :: FilePath -> IO (Either String [Module])
build root = do
  names <- getSortedModuleNames root
  case names of
    Left err -> return (Left err)
    Right ns -> do srcs <- zipWithM buildFile' [1..] ns
                   return (sequence srcs)
      where
        buildFile' n name = putStrLn (msg n name) >> buildFile name
        msg n name = "["++show n++" of "++show (length ns)++"] Compiling "++name

buildFile :: String -> IO (Either String Module)
buildFile moduleName =
  let filePath = toFilePath moduleName in
  case isNative moduleName of
    True  -> return (Right $ Module [moduleName] [] [] [])
             --return (Left "Can't do that yet")
             --Right `liftM` readFile filePath
    False -> do txt <- readFile filePath
                return $ buildFromSource txt


getSortedModuleNames :: FilePath -> IO (Either String [String])
getSortedModuleNames root = do
  deps <- readDeps [] root
  return (sortDeps =<< deps)

type Deps = (String, [String])

sortDeps :: [Deps] -> Either String [String]
sortDeps deps = go [] (nub deps)
    where
      msg = "A cyclical or missing module dependency or was detected in: "

      go :: [String] -> [Deps] -> Either String [String]
      go sorted [] = Right sorted
      go sorted unsorted =
          case partition (all (`elem` sorted) . snd) unsorted of
            ([],m:ms) -> Left (msg ++ intercalate ", " (map fst (m:ms)) ++ show sorted ++ show unsorted)
            (srtd,unsrtd) -> go (sorted ++ map fst srtd) unsrtd

readDeps :: [FilePath] -> FilePath -> IO (Either String [Deps])
readDeps seen root = do
  txt <- readFile root
  case preParse txt of
    Left err -> return (Left err)
    Right (name,deps) -> do rest <- mapM (readDeps seen' . toFilePath) newDeps
                            return $ do rs <- sequence rest
                                        return ((name, realDeps) : concat rs)
        where realDeps = filter (`notElem` builtIns) deps
              newDeps = filter (`notElem` seen) realDeps
              seen' = root : seen ++ newDeps
              builtIns = [] {--
                ["List","Char","Either","Maybe","Dict","Set","Date",
                 "Signal","Mouse","Keyboard.Raw","Keyboard","Touch",
                 "WebSocket","Window","Time","HTTP","Input","Random",
                 "Graphics","Text","Color","JavaScript","Automaton",
                 "JavaScript.Experimental","Prelude","JSON"]
                 --}

isNative name = takeWhile (/='.') name == "Native"
toFilePath name = map swapDots name ++ ext
    where swapDots '.' = '/'
          swapDots  c  =  c
          ext = if isNative name then ".js" else ".elm"
