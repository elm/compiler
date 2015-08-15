module Reporting.Result where

import Control.Applicative (Applicative, (<*>), (<|>), pure)
import qualified Control.Monad as M
import Control.Monad.Except (Except, runExcept)

import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import qualified Reporting.PrettyPrint as P


-- TASK

data Result warning error result =
    Result
      (Maybe P.Dealiaser, [A.Located warning])
      (RawResult [A.Located error] result)


data RawResult e a
    = Ok a
    | Err e


-- HELPERS

ok :: a -> Result w e a
ok value =
  Result (Nothing, []) (Ok value)


throw :: R.Region -> e -> Result w e a
throw region err =
  Result (Nothing, []) (Err [A.A region err])


throwMany :: [A.Located e] -> Result w e a
throwMany errors =
  Result (Nothing, []) (Err errors)


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


warn :: R.Region -> w -> Result w e ()
warn region warning =
  Result (Nothing, [A.A region warning]) (Ok ())


addWarnings :: [A.Located w] -> Result w e a -> Result w e a
addWarnings newWarnings (Result (dealiaser, warnings) rawResult) =
  Result (dealiaser, newWarnings ++ warnings) rawResult


addDealiaser :: P.Dealiaser -> Result w e a -> Result w e a
addDealiaser dealiaser (Result (_, warnings) rawResult) =
  Result (Just dealiaser, warnings) rawResult


destruct :: (e -> b) -> (a -> b) -> RawResult e a -> b
destruct errFunc okFunc rawResult =
  case rawResult of
    Ok answer ->
        okFunc answer

    Err errors ->
        errFunc errors


-- EXTRA FANCY HELPERS

instance M.Functor (Result w e) where
  fmap func (Result warnings rawResult) =
      case rawResult of
        Ok a ->
            Result warnings (Ok (func a))

        Err msgs ->
            Result warnings (Err msgs)


instance Applicative (Result w e) where
  pure value =
      ok value

  (<*>) (Result context resultFunc) (Result context' resultVal) =
      Result (merge context context') $
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

  (>>=) (Result context rawResult) callback =
      case rawResult of
          Err msg ->
              Result context (Err msg)

          Ok value ->
              let (Result context' rawResult') = callback value
              in
                  Result (merge context context') rawResult'


merge :: (Maybe a, [b]) -> (Maybe a, [b]) -> (Maybe a, [b])
merge (dealiaser, warnings) (dealiaser', warnings') =
  ( dealiaser <|> dealiaser'
  , warnings ++ warnings'
  )