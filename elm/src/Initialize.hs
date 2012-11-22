module Initialize (initialize) where

import Control.Applicative ((<$>))
import Control.Monad.Error
import Data.List (lookup)

import Ast
import Parser (parseProgram)
import Rename
import Types ((-:))
import Types.Hints (hints)
import Types.Unify
import Optimize


initialize str =
    let nameOf (Definition n _ _) = n in
    do modul@(Module name ex im stmts) <- parseProgram str
       (escapees, subs) <- unify hints modul
       let modul' = optimize . rename $ Module name ex im' stmts
               where im' | any ((=="Prelude") . fst) im = im
                         | otherwise = ("Prelude", Hiding []) : im
       subs `seq` return (escapees, modul')
