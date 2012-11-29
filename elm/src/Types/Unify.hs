
module Types.Unify (unify) where

import Control.Monad (liftM)
import qualified Data.Map as Map

import Guid
import Types.Constrain
import Types.Solver

unify hints modul = run $ do
  constraints <- constrain hints modul
  case constraints of
    Left msg -> return (Left msg)
    Right (escapees, cs) ->
        do subs <- solver cs Map.empty
           return ((,) escapees `liftM` subs)

