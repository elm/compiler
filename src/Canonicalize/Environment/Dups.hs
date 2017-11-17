{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Environment.Dups
  ( Dict
  , none
  , one
  , union
  , unions
  , insert
  , detect
  , checkFields
  )
  where


import qualified Data.Map as Map

import Canonicalize.Environment.Internals (Result)
import qualified Data.OneOrMore as OneOrMore
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result



-- INFO


type Dict category value =
  Map.Map N.Name (OneOrMore.OneOrMore (Info category value))


data Info category value =
  Info
    { _region :: R.Region
    , _category :: category
    , _value :: value
    }


none :: Dict c v
none =
  Map.empty


one :: N.Name -> R.Region -> category -> value -> Dict category value
one name region category value =
  Map.singleton name (OneOrMore.one (Info region category value))


union :: Dict c v -> Dict c v -> Dict c v
union a b =
  Map.unionWith OneOrMore.more a b


unions :: [Dict c v] -> Dict c v
unions dicts =
  Map.unionsWith OneOrMore.more dicts


insert :: N.Name -> R.Region -> c -> v -> Dict c v -> Dict c v
insert name region category value dict =
  Map.insertWith OneOrMore.more name (OneOrMore.one (Info region category value)) dict



-- DETECT


type ToError c =
  N.Name -> c -> c -> Error.Error


detect :: ToError c -> Dict c v -> Result () (Map.Map N.Name v)
detect toError dict =
  Map.traverseWithKey (detectHelp toError) dict


detectHelp :: ToError c -> N.Name -> OneOrMore.OneOrMore (Info c v) -> Result () v
detectHelp toError name values =
  case values of
    OneOrMore.One (Info _ _ value) ->
      Result.ok value

    OneOrMore.More _ _ ->
      let
        (Info r1 c1 _ : Info r2 c2 _ : _) =
          OneOrMore.toList values
      in
      Result.throw (max r1 r2) (toError name c1 c2)



-- CHECK FIELDS


checkFields :: [(A.Located N.Name, a)] -> Result () (Map.Map N.Name a)
checkFields fields =
  let
    toInfo (A.At region name, value) =
      Map.singleton name (OneOrMore.one (Info region () value))

    toError name () () =
      Error.DuplicateField name
  in
  detect toError $
    Map.unionsWith OneOrMore.more (map toInfo fields)
