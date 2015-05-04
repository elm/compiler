module Reporting.Result where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Control.Monad.Except (Except, runExcept)

import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


-- TASK

data Result warning error result =
    Result [A.Located warning] (RawResult [A.Located error] result)

data RawResult e a
    = Ok a
    | Err e


-- HELPERS

ok :: a -> Result w e a
ok value =
  Result [] (Ok value)


throw :: R.Region -> e -> Result w e a
throw region err =
  Result [] (Err [A.A region err])


throwMany :: [A.Located e] -> Result w e a
throwMany errors =
  Result [] (Err errors)


from :: (e -> e') -> Except [A.Located e] a -> Result w e' a
from f except =
  case runExcept except of
    Right answer ->
        ok answer

    Left errors ->
        throwMany (map (A.map f) errors)


mapError :: (e -> e') -> Result w e a -> Result w e' a
mapError f (Result warnings rawResult) =
  Result warnings $
    case rawResult of
      Ok v ->
          Ok v

      Err msgs ->
          Err (map (A.map f) msgs)


addWarnings :: [A.Located w] -> Result w e a -> Result w e a
addWarnings newWarnings (Result warnings rawResult) =
    Result (newWarnings ++ warnings) rawResult


-- EXTRA FANCY HELPERS

instance M.Functor (Result w e) where
  fmap func (Result warnings rawResult) =
      case rawResult of
        Ok a ->
            Result warnings (Ok (func a))

        Err msgs ->
            Result warnings (Err msgs)


instance A.Applicative (Result w e) where
  pure value =
      ok value

  (<*>) (Result warnings resultFunc) (Result warnings' resultVal) =
      Result (warnings ++ warnings') $
          case (resultFunc, resultVal) of
            (Ok func, Ok val) ->
                Ok (func val)

            (Err msgs, Err msgs') ->
                Err (msgs ++ msgs')

            (Err msgs, _) ->
                Err msgs

            (_, Err msgs) ->
                Err msgs


instance M.Monad (Result w e) where
  return value =
      ok value

  (>>=) (Result warnings rawResult) callback =
      case rawResult of
          Err msg ->
              Result warnings (Err msg)

          Ok value ->
              let (Result warnings' rawResult') = callback value
              in
                  Result (warnings ++ warnings') rawResult'