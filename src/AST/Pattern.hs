{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Pattern
  ( Raw
  , Raw_(..)
  , Canonical
  , Canonical_(..)
  , boundVars
  , member
  , boundVarSet
  , boundVarList
  )
  where


import qualified Data.Set as Set
import Data.Text (Text)

import qualified AST.Literal as L
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A



-- RAW


type Raw = A.Located Raw_


data Raw_
  = RCtor (Maybe Text) Text [Raw]
  | RRecord [Text]
  | RUnit
  | RTuple Raw Raw [Raw]
  | RList [Raw]
  | RCons Raw Raw
  | RAlias Text Raw
  | RVar Text
  | RAnything
  | RLiteral L.Literal



-- CANONICAL


type Canonical = A.Located Canonical_


data Canonical_
  = Ctor Var.Canonical [Canonical]
  | Record [Text]
  | Unit
  | Tuple Canonical Canonical [Canonical]
  | Alias Text Canonical
  | Var Text
  | Anything
  | Literal L.Literal



-- FIND VARIABLES


boundVars :: Raw -> [A.Located Text]
boundVars (A.A region pattern) =
  case pattern of
    RVar name ->
        [ A.A region name ]

    RAlias name realPattern ->
        A.A region name : boundVars realPattern

    RCtor _ _ patterns ->
        concatMap boundVars patterns

    RRecord fields ->
        map (A.A region) fields

    RUnit ->
        []

    RTuple a b cs ->
        concatMap boundVars (a:b:cs)

    RList xs ->
        concatMap boundVars xs

    RCons a b ->
        boundVars a ++ boundVars b

    RAnything ->
        []

    RLiteral _ ->
        []


member :: Text -> Raw -> Bool
member name pattern =
  elem name (map A.drop (boundVars pattern))


boundVarSet :: Raw -> Set.Set Text
boundVarSet pattern =
  Set.fromList (map A.drop (boundVars pattern))


boundVarList :: Raw -> [Text]
boundVarList pattern =
  Set.toList (boundVarSet pattern)
