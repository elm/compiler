
module Types.Unify (unify) where

import Control.Monad (liftM)
import qualified Data.Map as Map

import SourceSyntax.Module
import Unique
import qualified Types.Constrain as Constrain
import qualified Types.Solver as Solver
import qualified Types.Alias as Alias

import qualified Types.Substitutions as Subst
import System.IO.Unsafe
import Control.Arrow (second)

unify hints modul@(Module _ _ _ stmts) = run $ do
  result <- Constrain.constrain hints modul
  case result of
    Left err -> return (Left err)
    Right (schemes, constraints) ->
        do subs <- {- unsafePerformIO (mapM print constraints) `seq` -}
                   Solver.solver (Alias.get stmts) Map.empty constraints
           let ss = either (const []) Map.toList subs
           -- unsafePerformIO (mapM print . map (second (Subst.subst ss)) $ concatMap Map.toList schemes) `seq`
           return subs

