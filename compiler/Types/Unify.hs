
module Types.Unify (unify) where

import Control.Monad (liftM)
import qualified Data.Map as Map

import Guid
import Types.Constrain
import Types.Solver

unify hints modul = run $ do
  constraints <- constrain hints modul
  either (return . Left) (\cs -> solver cs Map.empty) constraints

