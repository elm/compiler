{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Environment.Dups
  ( Info(Info)
  , info
  , detect
  , checkFields
  )
  where


import qualified Data.Map as Map

import Canonicalize.Environment.Internals (Result)
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result



-- INFO


data Info category value =
  Info
    { _region :: R.Region
    , _category :: category
    , _value :: value
    }


{-# INLINE info #-}
info :: N.Name -> R.Region -> category -> value -> ( N.Name, [Info category value] )
info name region category value =
  ( name, [Info region category value] )



-- DETECT


type ToError c =
  N.Name -> c -> c -> Error.Error


detect :: ToError c -> [(N.Name, [Info c v])] -> Result () (Map.Map N.Name v)
detect toError pairs =
  Map.traverseWithKey (detectHelp toError) $ Map.fromListWith (++) pairs


detectHelp :: ToError c -> N.Name -> [Info c v] -> Result () v
detectHelp toError name values =
  case values of
    [Info _ _ value] ->
      Result.ok value

    [] ->
      error "impossible: use a non-empty data structure to rule this out"

    Info region1 category1 _ : Info region2 category2 _ : _ ->
      Result.throw (max region1 region2) (toError name category1 category2)



-- CHECK FIELDS


checkFields :: [(A.Located N.Name, a)] -> Result () (Map.Map N.Name a)
checkFields fields =
  let
    toInfo (A.A region name, value) =
      info name region () value

    toError name () () =
      Error.DuplicateField name
  in
  detect toError $ map toInfo fields
