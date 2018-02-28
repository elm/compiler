{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler.Imports
  ( addDefaults
  )
  where


import qualified AST.Source as Src
import qualified AST.Module.Name as Module
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- ADD DEFAULTS


addDefaults :: Pkg.Name -> [Src.Import] -> [Src.Import]
addDefaults pkgName imports =
  if pkgName == Pkg.core
    then imports
    else defaults ++ imports



-- DEFAULTS


defaults :: [Src.Import]
defaults =
  [ import_ Module.basics Nothing Src.Open
  , import_ Module.debug Nothing closed
  , import_ Module.list Nothing (operator "::")
  , import_ Module.maybe Nothing (typeOpen N.maybe)
  , import_ Module.result Nothing (typeOpen N.result)
  , import_ Module.string Nothing (typeClosed N.string)
  , import_ Module.char Nothing (typeClosed N.char)
  , import_ Module.tuple Nothing closed
  , import_ Module.platform Nothing (typeClosed N.program)
  , import_ Module.cmd (Just N.cmd) (typeClosed N.cmd)
  , import_ Module.sub (Just N.sub) (typeClosed N.sub)
  ]


import_ :: Module.Canonical -> Maybe N.Name -> Src.Exposing -> Src.Import
import_ (Module.Canonical _ name) maybeAlias exposing =
  Src.Import (A.At R.zero name) maybeAlias exposing



-- EXPOSING


closed :: Src.Exposing
closed =
  Src.Explicit []


typeOpen :: N.Name -> Src.Exposing
typeOpen name =
  Src.Explicit [ A.At R.zero (Src.Upper name Src.Public) ]


typeClosed :: N.Name -> Src.Exposing
typeClosed name =
  Src.Explicit [ A.At R.zero (Src.Upper name Src.Private) ]


operator :: N.Name -> Src.Exposing
operator op =
  Src.Explicit [ A.At R.zero (Src.Operator op) ]
