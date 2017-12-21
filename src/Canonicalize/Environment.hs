{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Environment
  ( Env(..)
  , VarHomes(..)
  , UnqualifiedVarHome(..)
  , ForeignVarHome(..)
  , Type(..)
  , Pattern(..)
  , Homes(..)
  , addLocals
  , findType
  , findPattern
  , findBinop
  , Binop(..)
  )
  where


import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map

import qualified AST.Utils.Binop as Binop
import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified Data.Bag as Bag
import qualified Data.Index as Index
import qualified Data.OneOrMore as OneOrMore
import qualified Elm.Name as N
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result



-- RESULT


type Result i w a =
  Result.Result i w Error.Error a



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
  = Local R.Region
  | TopLevel R.Region
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
    , _p_alts :: Can.CtorAlts
    , _p_index :: Index.ZeroBased
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



-- VARIABLE -- ADD LOCALS


addLocals :: Map.Map N.Name R.Region -> Env -> Result i w Env
addLocals names (Env home vars types patterns binops) =
  do  newVars <-
        Map.mergeA
          (Map.mapMissing addLocalLeft)
          (Map.mapMissing (\_ homes -> homes))
          (Map.zipWithAMatched addLocalBoth)
          names
          vars
      Result.ok $ Env home newVars types patterns binops


addLocalLeft :: N.Name -> R.Region -> VarHomes
addLocalLeft _ region =
  VarHomes (Local region) Map.empty


addLocalBoth :: N.Name -> R.Region -> VarHomes -> Result i w VarHomes
addLocalBoth name region (VarHomes unqualified qualified) =
  case unqualified of
    Foreign _ ->
      Result.ok (VarHomes (Local region) qualified)

    Local parentRegion ->
      Result.throw (Error.Shadowing name parentRegion region)

    TopLevel parentRegion ->
      Result.throw (Error.Shadowing name parentRegion region)



-- FIND TYPE and PATTERN


findType :: R.Region -> Env -> Maybe N.Name -> N.Name -> Result i w Type
findType region (Env _ _ types _ _) maybePrefix name =
  error "TODO findType" region types maybePrefix name


findPattern :: R.Region -> Env -> Maybe N.Name -> N.Name -> Result i w Pattern
findPattern region (Env _ _ _ patterns _) maybePrefix name =
  error "TODO findPattern" region patterns maybePrefix name



-- FIND BINOP


findBinop :: R.Region -> Env -> N.Name -> Result i w Binop
findBinop region (Env _ _ _ _ binops) name =
  case Map.lookup name binops of
    Just (OneOrMore.One binop) ->
      Result.ok binop

    Nothing ->
      Result.throw (error "TODO unknown binop" region)

    Just _ ->
      Result.throw (error "TODO ambiguous binop" region)
