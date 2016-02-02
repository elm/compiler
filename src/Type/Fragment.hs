{-# OPTIONS_GHC -Wall #-}
module Type.Fragment where

import qualified Data.List as List
import qualified Data.Map as Map

import Reporting.Annotation as A
import Type.Type


data Fragment = Fragment
    { typeEnv :: Map.Map String (A.Located Type)
    , vars :: [Variable]
    , typeConstraint :: TypeConstraint
    }


emptyFragment :: Fragment
emptyFragment =
    Fragment Map.empty [] CTrue


joinFragment :: Fragment -> Fragment -> Fragment
joinFragment f1 f2 =
    Fragment
      { typeEnv =
          Map.union (typeEnv f1) (typeEnv f2)

      , vars =
          vars f1 ++ vars f2

      , typeConstraint =
          typeConstraint f1 /\ typeConstraint f2
      }


joinFragments :: [Fragment] -> Fragment
joinFragments =
    List.foldl' (flip joinFragment) emptyFragment


toScheme :: Fragment -> Scheme Type Variable
toScheme fragment =
    Scheme [] (vars fragment) (typeConstraint fragment) (typeEnv fragment)
