{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Environment.Internals
  ( Result
  , Env(..)
  , VarHomes(..)
  , UnqualifiedVarHome(..)
  , ForeignVarHome(..)
  , Type(..)
  , Pattern(..)
  , Homes(..)
  , addLocals
  , findVar
  , findType
  , findPattern
  , findBinop
  , Binop(..)
  )
  where


import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Set as Set

import qualified AST.Utils.Binop as Binop
import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified Data.Bag as Bag
import qualified Data.OneOrMore as OneOrMore
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



-- RESULT HELPERS


type Result i a =
  Result.Result i Warning.Warning Error.Error a



-- ENVIRONMENT


data Env =
  Env
    { _home :: ModuleName.Canonical
    , _vars :: Map.Map N.Name VarHomes
    , _types :: Map.Map N.Name (Homes Type)
    , _patterns :: Map.Map N.Name (Homes Pattern)
    , _binops :: Map.Map N.Name (OneOrMore.OneOrMore Binop)
    }


data Homes a =
  Homes
    { _unqualified :: Bag.Bag a
    , _qualified :: Map.Map N.Name (OneOrMore.OneOrMore a)
    }



-- VARIABLES


data VarHomes =
  VarHomes
    { _var_unqualified :: UnqualifiedVarHome
    , _var_qualified :: Map.Map N.Name (OneOrMore.OneOrMore ForeignVarHome)
    }


data UnqualifiedVarHome
  = Local
  | TopLevel
  | Foreign (Bag.Bag ForeignVarHome)


data ForeignVarHome =
  ForeignVarHome ModuleName.Canonical Can.Annotation



-- TYPES


data Type
  = Alias Int ModuleName.Canonical [N.Name] Can.Type
  | Union Int ModuleName.Canonical



-- PATTERNs


data Pattern =
  Pattern
    { _p_home :: ModuleName.Canonical
    , _p_type :: N.Name
    , _p_vars :: [N.Name]
    , _p_args :: [Can.Type]
    }



-- BINOPS


data Binop =
  Binop
    { _op :: N.Name
    , _op_home :: ModuleName.Canonical
    , _op_name :: N.Name
    , _op_annotation :: Can.Annotation
    , _op_associativity :: Binop.Associativity
    , _op_precedence :: Binop.Precedence
    }



-- ADD LOCALS


addLocals :: Map.Map N.Name (A.Located a) -> Env -> Result () Env
addLocals names (Env home vars types patterns binops) =
  do  newVars <-
        Map.mergeA
          (Map.mapMissing (\_ _ -> localVarHomes))
          (Map.mapMissing (\_ homes -> homes))
          (Map.zipWithAMatched addLocalBoth)
          names
          vars
      Result.ok $ Env home newVars types patterns binops


{-# NOINLINE localVarHomes #-}
localVarHomes :: VarHomes
localVarHomes =
  VarHomes Local Map.empty


addLocalBoth :: N.Name -> A.Located a -> VarHomes -> Result () VarHomes
addLocalBoth name (A.At region _) (VarHomes unqualified qualified) =
  case unqualified of
    Foreign _ ->
      Result.ok (VarHomes Local qualified)

    Local ->
      Result.throw region (Error.Shadowing name)

    TopLevel ->
      Result.throw region (Error.Shadowing name)



-- FIND VAR


findVar :: R.Region -> Env -> Maybe N.Name -> N.Name -> Result (Set.Set N.Name) Can.Expr_
findVar region (Env localHome vars _ _ _) maybePrefix name =
  case Map.lookup name vars of
    Nothing ->
      Result.throw region (error "TODO no name")

    Just (VarHomes unqualified qualified) ->
      case maybePrefix of
        Nothing ->
          case unqualified of
            Local ->
              Result.accumulate (Set.singleton name) (Can.VarLocal name)

            TopLevel ->
              Result.accumulate (Set.singleton name) (Can.VarTopLevel localHome name)

            Foreign bag ->
              case bag of
                Bag.One (ForeignVarHome home annotation) ->
                  Result.ok (Can.VarForeign home name annotation)

                Bag.Empty ->
                  Result.throw region (error "TODO no unqualified")

                Bag.Two _ _ ->
                  Result.throw region (error "TODO ambiguous unqualified")

        Just prefix ->
          case Map.lookup prefix qualified of
            Nothing ->
              Result.throw region (error "TODO no qualified")

            Just (OneOrMore.One (ForeignVarHome home annotation)) ->
              Result.ok (Can.VarForeign home name annotation)

            Just _ ->
              Result.throw region (error "TODO ambiguous qualified")



-- FIND TYPE and PATTERN


findType :: R.Region -> Env -> Maybe N.Name -> N.Name -> Result () Type
findType region (Env _ _ types _ _) maybePrefix name =
  error "TODO findType" region types maybePrefix name


findPattern :: (Monoid i) => R.Region -> Env -> Maybe N.Name -> N.Name -> Result i Pattern
findPattern region (Env _ _ _ patterns _) maybePrefix name =
  error "TODO findPattern" region patterns maybePrefix name



-- FIND BINOP


findBinop :: R.Region -> Env -> N.Name -> Result (Set.Set N.Name) Binop
findBinop region (Env _ _ _ _ binops) name =
  case Map.lookup name binops of
    Just (OneOrMore.One binop) ->
      Result.ok binop

    Nothing ->
      Result.throw region (error "TODO unknown binop")

    Just _ ->
      Result.throw region (error "TODO ambiguous binop")
