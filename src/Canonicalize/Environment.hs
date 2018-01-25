{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Environment
  ( Env(..)
  , Exposed
  , Qualified
  , Var(..)
  , Type(..)
  , Ctor(..)
  , addLocals
  , findType
  , findTypeQual
  , findCtor
  , findCtorQual
  , findBinop
  , Binop(..)
  )
  where


import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict.Internal as I

import qualified AST.Utils.Binop as Binop
import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified Data.Index as Index
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
    , _vars :: Map.Map N.Name Var
    , _types :: Exposed Type
    , _ctors :: Exposed Ctor
    , _binops :: Exposed Binop
    , _q_vars :: Qualified Can.Annotation
    , _q_types :: Qualified Type
    , _q_ctors :: Qualified Ctor
    }


type Exposed a =
  Map.Map N.Name (Map.Map ModuleName.Canonical a)


type Qualified a =
  Map.Map N.Name (Map.Map N.Name (Map.Map ModuleName.Canonical a))



-- VARIABLES


data Var
  = Local R.Region
  | TopLevel R.Region
  | Foreign (Map.Map ModuleName.Canonical Can.Annotation)



-- TYPES


data Type
  = Alias Int ModuleName.Canonical [N.Name] Can.Type
  | Union Int ModuleName.Canonical



-- CTORS


data Ctor
  = RecordCtor ModuleName.Canonical [N.Name] Can.Type
  | Ctor
      { _c_home :: ModuleName.Canonical
      , _c_type :: N.Name
      , _c_union :: Can.Union
      , _c_index :: Index.ZeroBased
      , _c_args :: [Can.Type]
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
addLocals names (Env home vars ts cs bs qvs qts qcs) =
  do  newVars <-
        Map.mergeA
          (Map.mapMissing addLocalLeft)
          (Map.mapMissing (\_ homes -> homes))
          (Map.zipWithAMatched addLocalBoth)
          names
          vars

      Result.ok (Env home newVars ts cs bs qvs qts qcs)


addLocalLeft :: N.Name -> R.Region -> Var
addLocalLeft _ region =
  Local region


addLocalBoth :: N.Name -> R.Region -> Var -> Result i w Var
addLocalBoth name region var =
  case var of
    Foreign _ ->
      Result.ok (Local region)

    Local parentRegion ->
      Result.throw (Error.Shadowing name parentRegion region)

    TopLevel parentRegion ->
      Result.throw (Error.Shadowing name parentRegion region)



-- FIND TYPE


findType :: R.Region -> Env -> N.Name -> Result i w Type
findType region (Env _ _ types _ _ _ _ _) name =
  case Map.lookup name types of
    Just (I.Bin 1 _ tipe _ _) ->
      Result.ok tipe

    Just _ ->
      error "TODO findType ambiguous unqualified"

    Nothing ->
      error ("TODO findType not found: " ++ N.toString name) region


findTypeQual :: R.Region -> Env -> N.Name -> N.Name -> Result i w Type
findTypeQual region (Env _ _ _ _ _ _ types _) prefix name =
  case Map.lookup prefix types of
    Just qualified ->
      case Map.lookup name qualified of
        Just (I.Bin 1 _ tipe _ _) ->
          Result.ok tipe

        Just more ->
          error "TODO findTypeQual ambiguous qualified" more

        Nothing ->
          error "TODO findTypeQual no qualified"

    Nothing ->
      error "TODO findTypeQual not found" region



-- FIND CTOR


findCtor :: R.Region -> Env -> N.Name -> Result i w Ctor
findCtor region (Env _ _ _ ctors _ _ _ _) name =
  case Map.lookup name ctors of
    Just (I.Bin 1 _ ctor _ _) ->
      Result.ok ctor

    Just _ ->
      error ("TODO findCtor ambiguous unqualified " ++ N.toString name)

    Nothing ->
      error ("TODO findCtor not found " ++ N.toString name) region


findCtorQual :: R.Region -> Env -> N.Name -> N.Name -> Result i w Ctor
findCtorQual region (Env _ _ _ _ _ _ _ ctors) prefix name =
  case Map.lookup prefix ctors of
    Just qualified ->
      case Map.lookup name qualified of
        Just (I.Bin 1 _ pattern _ _) ->
          Result.ok pattern

        Just _ ->
          error "TODO findCtorQual ambiguous qualified"

        Nothing ->
          error "TODO findCtorQual no qualified"

    Nothing ->
      error "TODO findCtorQual not found" region



-- FIND BINOP


findBinop :: R.Region -> Env -> N.Name -> Result i w Binop
findBinop region (Env _ _ _ _ binops _ _ _) name =
  case Map.lookup name binops of
    Just (I.Bin 1 _ binop _ _) ->
      Result.ok binop

    Nothing ->
      Result.throw (error ("TODO unknown binop " ++ N.toString name) region)

    Just _ ->
      Result.throw (error ("TODO ambiguous binop " ++ N.toString name) region)
