
module Types.Unify (unify) where

import Control.Monad (liftM)
import qualified Data.Map as Map

import Ast
import Guid
import Types.Constrain
import Types.Solver
import Types.Alias as Alias

unify hints modul@(Module _ _ _ stmts) = run $ do
  constraints <- constrain hints modul
  either (return . Left) (solver (Alias.get stmts) Map.empty) constraints

