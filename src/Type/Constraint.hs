{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Constraint
  ( Constraint(..)
  , (/\), ex, forall
  , exists, existsNumber
  , Scheme(..), SchemeName
  , monoscheme
  )
  where


import qualified Data.Map.Strict as Map

import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Reporting.Region as R
import Type.Type (Type(VarN), Variable, mkFlexVar, mkFlexNumber)



-- CONSTRAINTS


data Constraint
    = CTrue
    | CSaveEnv
    | CEqual Error.Hint R.Region Type Type
    | CAnd [Constraint]
    | CLet [Scheme] Constraint
    | CInstance R.Region SchemeName Type



-- SCHEMES


type SchemeName = N.Name


data Scheme =
  Scheme
    { _rigidQuantifiers :: [Variable]
    , _flexibleQuantifiers :: [Variable]
    , _constraint :: Constraint
    , _header :: Map.Map N.Name (A.Located Type)
    }



-- SCHEME HELPERS


monoscheme :: Map.Map N.Name (A.Located Type) -> Scheme
monoscheme headers =
  Scheme [] [] CTrue headers



-- CONSTRAINT HELPERS


infixl 8 /\


(/\) :: Constraint -> Constraint -> Constraint
(/\) c1 c2 =
    case (c1, c2) of
      (CTrue, _) -> c2
      (_, CTrue) -> c1
      _ -> CAnd [c1,c2]


-- ex qs constraint == exists qs. constraint
ex :: [Variable] -> Constraint -> Constraint
ex fqs constraint =
    CLet [Scheme [] fqs constraint Map.empty] CTrue


forall :: [Variable] -> Constraint -> Constraint
forall rqs constraint =
    CLet [Scheme rqs [] constraint Map.empty] CTrue


exists :: (Type -> IO Constraint) -> IO Constraint
exists f =
  do  v <- mkFlexVar
      ex [v] <$> f (VarN v)


existsNumber :: (Type -> IO Constraint) -> IO Constraint
existsNumber f =
  do  v <- mkFlexNumber
      ex [v] <$> f (VarN v)
