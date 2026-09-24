{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Environment.Dups
  ( detect
  , checkFields
  , checkFields'
  , Dict
  , none
  , one
  , insert
  , union
  , unions
  )
  where


import qualified Data.Map as Map

import qualified AST.Prim.Name as N
import qualified Data.OneOrMore as OneOrMore
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Result as Result



-- DUPLICATE TRACKER


type Dict name value =
  Map.Map name (OneOrMore.OneOrMore (Info value))


data Info value =
  Info
    { _region :: A.Region
    , _value :: value
    }



-- DETECT


type ToError name =
  name -> A.Region -> A.Region -> Error.Error


detect :: ToError k -> Dict k a -> Result.Result i w Error.Error (Map.Map k a)
detect toError dict =
  Map.traverseWithKey (detectHelp toError) dict


detectHelp :: ToError name -> name -> OneOrMore.OneOrMore (Info a) -> Result.Result i w Error.Error a
detectHelp toError name values =
  case values of
    OneOrMore.One (Info _ value) ->
      return value

    OneOrMore.More left right ->
      let
        (Info r1 _, Info r2 _) =
          OneOrMore.getFirstTwo left right
      in
      Result.throw (toError name r1 r2)



-- CHECK FIELDS


checkFields :: [(A.Located N.Name, a)] -> Result.Result i w Error.Error (Map.Map N.Name a)
checkFields fields =
  detect Error.DuplicateField (foldr addField none fields)


addField :: (A.Located N.Name, a) -> Dict N.Name a -> Dict N.Name a
addField (A.At region name, value) dups =
  Map.insertWith OneOrMore.more name (OneOrMore.one (Info region value)) dups


checkFields' :: (A.Region -> a -> b) -> [(A.Located N.Name, a)] -> Result.Result i w Error.Error (Map.Map N.Name b)
checkFields' toValue fields =
  detect Error.DuplicateField (foldr (addField' toValue) none fields)


addField' :: (A.Region -> a -> b) -> (A.Located N.Name, a) -> Dict N.Name b -> Dict N.Name b
addField' toValue (A.At region name, value) dups =
  Map.insertWith OneOrMore.more name (OneOrMore.one (Info region (toValue region value))) dups



-- BUILDING DICTIONARIES


none :: Dict k v
none =
  Map.empty


one :: k -> A.Region -> v -> Dict k v
one name region value =
  Map.singleton name (OneOrMore.one (Info region value))


insert :: (Ord k) => k -> A.Region -> v -> Dict k v -> Dict k v
insert name region value dict =
  Map.insertWith (\new old -> OneOrMore.more old new) name (OneOrMore.one (Info region value)) dict


union :: (Ord k) => Dict k v -> Dict k v -> Dict k v
union a b =
  Map.unionWith OneOrMore.more a b


unions :: (Ord k) => [Dict k v] -> Dict k v
unions dicts =
  Map.unionsWith OneOrMore.more dicts
