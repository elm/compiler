module Reporting.Task where

import Control.Monad.Except (Except, ExceptT, runExcept, runExceptT, throwError, withExceptT)
import qualified Control.Monad.State as State

import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


-- TASK

type Task warning error result =
    ExceptT [A.Located error] (State.State [A.Located warning]) result


-- RUN TASKS

run :: Task w e a -> (Either [A.Located e] a, [A.Located w])
run task =
  State.runState (runExceptT task) []


-- WARNINGS

addWarnings :: [A.Located w] -> Task w e ()
addWarnings warnings =
  State.modify (warnings++)


-- ERRORS

throw :: R.Region -> e -> Task w e a
throw region err =
  throwError [A.A region err]


mapError :: (e -> e') -> Task w e a -> Task w e' a
mapError f task =
  withExceptT (map (A.map f)) task


from :: (e -> e') -> Except [A.Located e] a -> Task w e' a
from f except =
  case runExcept except of
    Right answer ->
        return answer

    Left errors ->
        throwError (map (A.map f) errors)


