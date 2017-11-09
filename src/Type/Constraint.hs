{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Constraint
  ( Constraint(..)
  , ex
  , Scheme(..)
  , monoscheme
  )
  where


import qualified Data.Map.Strict as Map

import qualified AST.Canonical as Can
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Reporting.Region as R
import Type.Type (Type, Variable)



-- CONSTRAINTS


data Constraint
    = CTrue
    | CSaveHeaders
    | CEqual R.Region Error.Context Error.Category R.Region Type Type
    | CLookup R.Region Error.Context R.Region N.Name Type
    | CInstance R.Region Error.Context R.Region Can.Type Type
    | CPattern R.Region Error.PatternContext Error.PatternCategory R.Region Type Type
    | CAnd [Constraint]
    | CLet [Scheme] Constraint



-- SCHEMES


data Scheme =
  Scheme
    { _rigidVars :: [Variable]
    , _flexVars :: [Variable]
    , _header :: Map.Map N.Name (A.Located Type)
    , _constraint :: Constraint
    }



-- SCHEME HELPERS


monoscheme :: Map.Map N.Name (A.Located Type) -> Scheme
monoscheme headers =
  Scheme [] [] headers CTrue



-- CONSTRAINT HELPERS


-- ex qs constraint == exists qs. constraint
ex :: [Variable] -> Constraint -> Constraint
ex flexVars constraint =
    CLet
      [ Scheme [] flexVars Map.empty constraint
      ]
      CTrue
