{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Result where

import Prelude hiding (foldl)
import qualified Control.Applicative as A
import qualified Data.Functor as F
import qualified Data.Set as Set

import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error


type ResultErr a = Result (A.Located Error.Error) a


data Result err a =
    Result (Set.Set ModuleName.Raw) (RawResult err a)


data RawResult err a
    = Ok a
    | Err [err]


-- RESULT HELPERS

ok :: a -> Result e a
ok value =
  Result Set.empty (Ok value)


err :: e -> Result e a
err msg =
  errors [msg]


errors :: [e] -> Result e a
errors msgs =
  Result Set.empty (Err msgs)


foldl :: (a -> b -> Result e b) -> b -> [a] -> Result e b
foldl f acc list =
  case list of
    [] ->
        ok acc

    x:xs ->
        f x acc `andThen` (\acc' -> foldl f acc' xs)


andThen :: Result e a -> (a -> Result e b) -> Result e b
andThen (Result uses rawResult) callback =
  case rawResult of
    Err msg ->
        Result uses (Err msg)

    Ok value ->
        let (Result uses' rawResult') = callback value
        in
            Result (Set.union uses uses') rawResult'


instance F.Functor (Result e) where
  fmap func (Result uses result) =
      case result of
        Ok a ->
            Result uses (Ok (func a))

        Err msgs ->
            Result uses (Err msgs)


instance A.Applicative (Result e) where
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

addModule :: ModuleName.Raw -> Result e a -> Result e a
addModule home (Result uses result) =
  Result (Set.insert home uses) result


var' :: (a -> Var.Canonical) -> a -> Result e a
var' toVar value =
  case toVar value of
    Var.Canonical (Var.Module (ModuleName.Canonical _ moduleName)) _name ->
        Result (Set.singleton moduleName) (Ok value)

    Var.Canonical _ _ ->
        ok value


var :: Var.Canonical -> Result e Var.Canonical
var variable =
  var' id variable


