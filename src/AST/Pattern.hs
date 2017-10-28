{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Pattern
  ( Raw
  , Raw_(..)
  , Canonical
  , Canonical_(..)
  )
  where


import qualified AST.Literal as Literal
import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- RAW


type Raw = A.Located Raw_


data Raw_
  = RCtor R.Region (Maybe N.Name) N.Name [Raw]
  | RRecord [A.Located N.Name]
  | RUnit
  | RTuple Raw Raw [Raw]
  | RList [Raw]
  | RCons Raw Raw
  | RAlias Raw (A.Located N.Name)
  | RVar N.Name
  | RAnything
  | RLiteral Literal.Literal



-- CANONICAL


type Canonical = A.Located Canonical_


data Canonical_
  = Ctor ModuleName.Canonical N.Name [Canonical]
  | Record [N.Name]
  | Unit
  | Tuple Canonical Canonical [Canonical]
  | Alias Canonical N.Name
  | Var N.Name
  | Anything
  | Literal Literal.Literal
