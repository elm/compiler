{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Environment.Internals
  ( Result
  , Env(..)
  , VarHomes(..)
  , UnqualifiedVarHome(..)
  , Type(..)
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

import qualified AST.Binop as Binop
import qualified AST.Expression.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type
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
    , _types :: Map.Map N.Name (Homes (Type, Int))
    , _patterns :: Map.Map N.Name (Homes (ModuleName.Canonical, Int))
    , _binops :: Map.Map N.Name (OneOrMore.OneOrMore Binop)
    }


data Homes a =
  Homes
    { _unqualified :: Bag.Bag a
    , _qualified :: Map.Map N.Name (OneOrMore.OneOrMore a)
    }


data VarHomes =
  VarHomes
    { _var_unqualified :: UnqualifiedVarHome
    , _var_qualified :: Map.Map N.Name (OneOrMore.OneOrMore ModuleName.Canonical)
    }


data UnqualifiedVarHome
  = Local
  | TopLevel
  | Foreign (Bag.Bag ModuleName.Canonical)


data Type
  = Alias ModuleName.Canonical [N.Name] Type.Canonical
  | Union ModuleName.Canonical


data Binop =
  Binop
    { _op_name :: N.Name
    , _op_home :: ModuleName.Canonical
    , _op_func :: N.Name
    , _associativity :: Binop.Associativity
    , _precedence :: Binop.Precedence
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
addLocalBoth name (A.A region _) (VarHomes unqualified qualified) =
  case unqualified of
    Foreign _ ->
      Result.ok (VarHomes Local qualified)

    Local ->
      Result.throw region (Error.Shadowing name)

    TopLevel ->
      Result.throw region (Error.Shadowing name)



-- FIND VAR


findVar :: R.Region -> Env -> Maybe N.Name -> N.Name -> Result (Set.Set N.Name) Can.Expr_
findVar region (Env home vars _ _ _) maybePrefix name =
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
              Result.accumulate (Set.singleton name) (Can.VarTopLevel home name)

            Foreign bag ->
              case bag of
                Bag.One foreignHome ->
                  Result.ok (Can.VarForeign foreignHome name)

                Bag.Empty ->
                  Result.throw region (error "TODO no unqualified")

                Bag.Two _ _ ->
                  Result.throw region (error "TODO ambiguous unqualified")

        Just prefix ->
          case Map.lookup prefix qualified of
            Nothing ->
              Result.throw region (error "TODO no qualified")

            Just (OneOrMore.One foreignHome) ->
              Result.ok (Can.VarForeign foreignHome name)

            Just _ ->
              Result.throw region (error "TODO ambiguous qualified")



-- FIND TYPE and PATTERN


findType :: (Monoid i) => R.Region -> Env -> Maybe N.Name -> N.Name -> Int -> Result i Type
findType region (Env _ _ types _ _) maybePrefix name arity =
  error "TODO findType" region types maybePrefix name arity


findPattern :: (Monoid i) => R.Region -> Env -> Maybe N.Name -> N.Name -> Int -> Result i ModuleName.Canonical
findPattern region (Env _ _ _ patterns _) maybePrefix name arity =
  error "TODO findPattern" region patterns maybePrefix name arity



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
