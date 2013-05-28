
module Types.Unify (unify) where

import Control.Monad (liftM)
import qualified Data.Map as Map

import Ast
import Guid
import qualified Types.Constrain as Constrain
import qualified Types.Solver as Solver
import qualified Types.Alias as Alias

unify hints modul@(Module _ _ _ stmts) = run $ do
  constraints <- Constrain.constrain hints modul
  either (return . Left) (Solver.solver (Alias.get stmts) Map.empty) constraints

