{-# OPTIONS_GHC -Wall #-}
module Transform.Canonicalize.Result where

import Prelude hiding (foldl)
import qualified Control.Applicative as A
import qualified Data.Functor as F
import qualified Data.Set as Set

import qualified AST.Module as Module
import qualified AST.Variable as Var
import qualified Transform.Canonicalize.Error as Error


data Result a =
    Result (Set.Set Module.Name) (RawResult Error.Error a)


data RawResult err a
    = Ok a
    | Err [err]


-- RESULT HELPERS

ok :: a -> Result a
ok value =
  Result Set.empty (Ok value)


err :: Error.Error -> Result a
err msg =
  errors [msg]


errors :: [Error.Error] -> Result a
errors msgs =
  Result Set.empty (Err msgs)


foldl :: (a -> b -> Result b) -> b -> [a] -> Result b
foldl f acc list =
  case list of
    [] ->
        ok acc

    x:xs ->
        f x acc `andThen` (\acc' -> foldl f acc' xs)


andThen :: Result a -> (a -> Result b) -> Result b
andThen (Result uses rawResult) callback =
  case rawResult of
    Err msg ->
        Result uses (Err msg)

    Ok value ->
        callback value


instance F.Functor Result where
  fmap func (Result uses result) =
      case result of
        Ok a ->
            Result uses (Ok (func a))

        Err msgs ->
            Result uses (Err msgs)


instance A.Applicative Result where
  pure value =
      ok value

  (<*>) (Result uses resultFunc) (Result uses' resultVal) =
      Result (Set.union uses uses') $
          case (resultFunc, resultVal) of
            (Ok func, Ok val) ->
                Ok (func val)

            (Err msgs, Err msgs') ->
                Err (msgs ++ msgs')

            (Err msgs, _) ->
                Err msgs

            (_, Err msgs) ->
                Err msgs


-- TRACK USES OF IMPORTS

addModule :: Module.Name -> Result a -> Result a
addModule home (Result uses result) =
  Result (Set.insert home uses) result


var' :: (a -> Var.Canonical) -> a -> Result a
var' toVar value =
  case toVar value of
    Var.Canonical (Var.Module moduleName) _name ->
        Result (Set.singleton moduleName) (Ok value)

    Var.Canonical _ _ ->
        ok value


var :: Var.Canonical -> Result Var.Canonical
var variable =
  var' id variable


