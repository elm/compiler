module Initialize (build,buildFromSource) where

import Control.Applicative ((<$>))
import Control.Monad.Error
import Data.List (lookup)

import Ast
import Data.Either (lefts,rights)
import Data.List (intercalate,partition)
import Parse.Parser (parseProgram)
import Rename
import Types.Types ((-:))
import Types.Hints (hints)
import Types.Unify
import Types.Alias
import Optimize

checkMistakes :: Module -> Either String Module
checkMistakes modul@(Module name ex im stmts) = 
  case mistakes stmts of
    m:ms -> Left (unlines (m:ms))
    []   -> return modul

checkTypes :: Module -> Either String ([String], Module)
checkTypes (Module name ex im stmts) =
    let stmts' = dealias stmts
        modul = Module name ex im stmts'
    in do (escapees, subs) <- unify hints modul
          let im' | any ((=="Prelude") . fst) im = im
                  | otherwise = ("Prelude", Hiding []) : im
              modul' = optimize . renameModule $ Module name ex im' stmts'
          subs `seq` return (escapees, modul')

check :: Module -> Either String ([String], Module)
check = checkMistakes >=> checkTypes

buildFromSource :: String -> Either String ([String],Module)
buildFromSource = parseProgram >=> check

build :: Bool -> FilePath -> IO (Either String ([String],[Module]))
build make root = do
  proj <- parse make root
  return $ do
    ms <- proj
    (escapeess, modules) <- unzip `liftM` mapM check ms
    sortedModules <- (if make then sort else return) modules
    return (concat escapeess, sortedModules)

parse :: Bool -> FilePath -> IO (Either String [Module])
parse make root = parseProject [] make root

parseProject :: [FilePath] -> Bool -> FilePath -> IO (Either String [Module])
parseProject seen make root = do
  txt <- readFile root
  case parseProgram txt of
    Left err -> return (Left err)
    Right modul@(Module names exs ims stmts) ->
        let allDeps = map toFilePath (getDeps modul)
            newDeps = if make then filter (`notElem` seen) allDeps else []
            name    = intercalate "." names
        in  do ps <- mapM (parseProject (name : seen ++ newDeps) make) newDeps
               return $ case unlines (lefts ps) of
                          c:cs -> Left (c:cs)
                          _    -> Right (modul : concat (rights ps))

toFilePath :: String -> FilePath
toFilePath modul = map (\c -> if c == '.' then '/' else c) modul ++ ".elm"

getDeps :: Module -> [String]
getDeps (Module _ _ is _) = filter (`notElem` builtInModules) (map fst is)
  where 
    builtInModules =
        ["List","Char","Either","Maybe","Dict","Set","Automaton","Date",
         "Signal","Mouse","Keyboard.Raw","Keyboard","Touch",
         "WebSocket","Window","Time","HTTP","Input","Random",
         "Graphics","Text","Color","JavaScript",
         "JavaScript.Experimental","Prelude","JSON"]


sort :: [Module] -> Either String [Module]
sort = go []
    where
      msg = "A cyclical or missing module dependency or was detected in: "
      getName (Module names _ _ _) = intercalate "." names

      has name modul = name == getName modul
      within ms name = any (has name) ms

      go :: [Module] -> [Module] -> Either String [Module]
      go sorted [] = Right sorted
      go sorted unsorted =
          case partition (all (within sorted) . getDeps) unsorted of
            ([],m:ms) -> Left (msg ++ intercalate ", " (map getName (m:ms)))
            (srtd,unsrtd) -> go (sorted ++ srtd) unsrtd
      