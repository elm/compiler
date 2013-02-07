module Initialize (initialize) where

import Control.Applicative ((<$>))
import Control.Monad.Error
import Data.List (lookup)

import Ast
import Parse.Parser (parseProgram)
import Rename
import Types.Types ((-:))
import Types.Hints (hints)
import Types.Unify
import Types.Alias
import Optimize

initialize :: String -> Either String ([String], Module)
initialize str = do
  Module name ex im stmts <- parseProgram str
  case mistakes stmts of
    m:ms -> Left (unlines (m:ms))
    [] -> let stmts' = dealias stmts
              modul = Module name ex im stmts'
          in do (escapees, subs) <- unify hints modul
                let im' | any ((=="Prelude") . fst) im = im
                        | otherwise = ("Prelude", Hiding []) : im
                    modul' = optimize . renameModule $ Module name ex im' stmts'
                subs `seq` return (escapees, modul')
