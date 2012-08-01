module Initialize (initialize) where

import Control.Applicative ((<$>))
import Control.Monad.Error
import Data.List (lookup)

import Ast
import Parser (parseProgram)
import Hints (hints)
import Unify
import Rename
import Types ((-:))
import Optimize


initialize str =
    let nameOf (Definition n _ _) = n in
    do modul@(Module name ex im stmts) <- rename <$> parseProgram str
       subs <- unify hints modul
       let modul' = Module name ex im' stmts
               where im' | any ((=="Prelude") . fst) im = im
                         | otherwise = ("Prelude", Importing []) : im
       subs `seq` return (optimize modul')
