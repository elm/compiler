{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Constraint
  ( Constraint(..)
  , exists
  , Expected(..)
  , Context(..)
  , SubContext(..)
  , FuncName(..)
  , Category(..)
  , PExpected(..)
  , PContext(..)
  , PCategory(..)
  , typeReplace
  , ptypeReplace
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
  | CSaveTheEnvironment
  | CEqual R.Region Category Type (Expected Type)
  | CLookup R.Region N.Name (Expected Type)
  | CInstance R.Region FuncName Can.Annotation (Expected Type)
  | CPattern R.Region PCategory Type (PExpected Type)
  | CAnd [Constraint]
  | CBranch
      { _a :: Type
      , _b :: Type
      , _eq :: Constraint
      , _neq :: IO Constraint
      }
  | CLet
      { _rigidVars :: [Variable]
      , _flexVars :: [Variable]
      , _header :: Map.Map N.Name (A.Located Type)
      , _headerCon :: Constraint
      , _bodyCon :: Constraint
      }


exists :: [Variable] -> Constraint -> Constraint
exists flexVars constraint =
  CLet [] flexVars Map.empty constraint CTrue



-- EXPRESSION EXPECTATIONS


data Expected tipe
  = NoExpectation tipe
  | FromContext R.Region Context tipe
  | FromAnnotation N.Name Int SubContext tipe


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


data PExpected tipe
  = PNoExpectation tipe
  | PFromContext R.Region PContext tipe


data PContext
  = PTypedArg N.Name Index.ZeroBased
  | PCaseMatch Index.ZeroBased
  | PCtorArg N.Name Index.ZeroBased
  | PListEntry Index.ZeroBased
  | PTail


data PCategory
  = PRecord
  | PUnit
  | PTuple
  | PList
  | PCtor N.Name
  | PInt
  | PStr
  | PChr



-- HELPERS


typeReplace :: Expected Type -> Can.Type -> Expected Can.Type
typeReplace expectation tipe =
  case expectation of
    NoExpectation _ ->
      NoExpectation tipe

    FromContext region context _ ->
      FromContext region context tipe

    FromAnnotation name arity context _ ->
      FromAnnotation name arity context tipe


ptypeReplace :: PExpected Type -> Can.Type -> PExpected Can.Type
ptypeReplace expectation tipe =
  case expectation of
    PNoExpectation _ ->
      PNoExpectation tipe

    PFromContext region context _ ->
      PFromContext region context tipe
