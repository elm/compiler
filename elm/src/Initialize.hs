module Initialize (initialize) where

import Control.Monad.Error

import Ast
import Parser (parseProgram)
import Hints
import Unify
import Rename
import Optimize

initialize str = do
  (Module name ex im defs, tipes) <- parseProgram str
  let expr = rename . Let defs $ Var "_"
  subs <- unify (liftM2 (++) hints tipes) expr
  let Let defs' _ = optimize expr
  let im' = if any ((=="Prelude") . fst) im then im else ("Prelude", Importing []):im
  return (subs `seq` Module name ex im' defs')
