{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Environment.Dups
  ( detect
  , checkFields
  , Dict
  , none
  , one
  , insert
  , union
  , unions
  )
  where


import qualified Data.Map as Map

import qualified Canonicalize.Result as Result
import qualified Data.OneOrMore as OneOrMore
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R



-- DUPLICATE TRACKER


type Dict category value =
  Map.Map N.Name (OneOrMore.OneOrMore (Info category value))


data Info category value =
  Info
    { _region :: R.Region
    , _category :: category
    , _value :: value
    }



-- DETECT


type ToError c =
  N.Name -> R.Region -> c -> R.Region -> c -> Error.Error


detect :: ToError c -> Dict c v -> Result.Result i w Error.Error (Map.Map N.Name v)
detect toError dict =
  Map.traverseWithKey (detectHelp toError) dict


detectHelp :: ToError c -> N.Name -> OneOrMore.OneOrMore (Info c v) -> Result.Result i w Error.Error v
detectHelp toError name values =
  case values of
    OneOrMore.One (Info _ _ value) ->
      return value

    OneOrMore.More _ _ ->
      let (Info r1 c1 _ : Info r2 c2 _ : _) = OneOrMore.toList values in
      Result.throw (toError name r1 c1 r2 c2)



-- CHECK FIELDS


checkFields :: [(A.Located N.Name, a)] -> Result.Result i w Error.Error (Map.Map N.Name a)
checkFields fields =
  detect Error.DuplicateField (foldr addField none fields)


addField :: (A.Located N.Name, a) -> Dict () a -> Dict () a
addField (A.At region name, value) dups =
  Map.insertWith OneOrMore.more name (OneOrMore.one (Info region () value)) dups



-- BUILDING DICTIONARIES


none :: Dict c v
none =
  Map.empty


one :: N.Name -> R.Region -> category -> value -> Dict category value
one name region category value =
  Map.singleton name (OneOrMore.one (Info region category value))


insert :: N.Name -> R.Region -> c -> v -> Dict c v -> Dict c v
insert name region category value dict =
  Map.insertWith OneOrMore.more name (OneOrMore.one (Info region category value)) dict


union :: Dict c v -> Dict c v -> Dict c v
union a b =
  Map.unionWith OneOrMore.more a b


unions :: [Dict c v] -> Dict c v
unions dicts =
  Map.unionsWith OneOrMore.more dicts
