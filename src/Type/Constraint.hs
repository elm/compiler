{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Constraint
  ( Constraint(..)
  , exists
  , Expectation(..)
  , Context(..)
  , SubContext(..)
  , FuncName(..)
  , Category(..)
  , PatternExpectation(..)
  , PatternContext(..)
  , PatternCategory(..)
  )
  where


import qualified Data.Map.Strict as Map

import qualified AST.Canonical as Can
import qualified Data.Index as Index
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import Type.Type (Type, Variable)



-- CONSTRAINTS


data Constraint
  = CTrue
  | CSaveHeaders
  | CEqual R.Region Category Type Expectation
  | CLookup R.Region N.Name Expectation
  | CInstance R.Region FuncName Can.Annotation Expectation
  | CPattern R.Region PatternCategory Type PatternExpectation
  | CAnd [Constraint]
  | CLet
      { _rigidVars :: [Variable]
      , _flexVars :: [Variable]
      , _header :: Map.Map N.Name (A.Located Type)
      , _headerCon :: Constraint
      , _bodyCon :: Constraint
      }
  | CBranch
      { _a :: Type
      , _b :: Type
      , _eq :: Constraint
      , _neq :: IO Constraint
      }


exists :: [Variable] -> Constraint -> Constraint
exists flexVars constraint =
  CLet [] flexVars Map.empty constraint CTrue



-- EXPRESSION EXPECTATIONS


data Expectation
  = NoExpectation Type
  | FromContext R.Region Context Type
  | FromAnnotation N.Name Int SubContext Type


data Context
  = ListEntry Index.ZeroBased
  | Negate
  | OpLeft N.Name
  | OpRight N.Name
  | IfCondition
  | IfBranch Index.ZeroBased
  | CaseBranch Index.ZeroBased
  | CallArg (Maybe FuncName) Index.ZeroBased
  | BadArity
  | RecordAccess N.Name
  | RecordUpdate
  | Destructure


data SubContext
  = TypedIfBranch Index.ZeroBased
  | TypedCaseBranch Index.ZeroBased
  | TypedBody


data FuncName
  = FuncName N.Name
  | OpName N.Name


data Category
  = List
  | Number
  | Float
  | String
  | Char
  | Binop
  | If
  | Case
  | CallResult (Maybe FuncName)
  | Lambda
  | Accessor N.Name
  | Access N.Name
  | Record
  | Tuple
  | Unit
  | Shader



-- PATTERN EXPECTATIONS


data PatternExpectation
  = NoPatternExpectation Type
  | PatternExpectation R.Region PatternContext Type


data PatternContext
  = PTypedArg N.Name Index.ZeroBased
  | PCaseMatch Index.ZeroBased
  | PCtorArg N.Name Index.ZeroBased
  | PListEntry Index.ZeroBased
  | PTail


data PatternCategory
  = PRecord
  | PUnit
  | PTuple
  | PList
  | PCtor N.Name
  | PInt
  | PStr
  | PChr
