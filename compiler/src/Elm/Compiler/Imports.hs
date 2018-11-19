{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler.Imports
  ( addDefaults
  )
  where


import qualified Data.Name as Name

import qualified AST.Source as Src
import qualified AST.Module.Name as Module
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
  , import_ Module.maybe Nothing (typeOpen Name.maybe)
  , import_ Module.result Nothing (typeOpen Name.result)
  , import_ Module.string Nothing (typeClosed Name.string)
  , import_ Module.char Nothing (typeClosed Name.char)
  , import_ Module.tuple Nothing closed
  , import_ Module.platform Nothing (typeClosed Name.program)
  , import_ Module.cmd (Just Name.cmd) (typeClosed Name.cmd)
  , import_ Module.sub (Just Name.sub) (typeClosed Name.sub)
  ]


import_ :: Module.Canonical -> Maybe Name.Name -> Src.Exposing -> Src.Import
import_ (Module.Canonical _ name) maybeAlias exposing =
  Src.Import (A.At R.zero name) maybeAlias exposing



-- EXPOSING


closed :: Src.Exposing
closed =
  Src.Explicit []


typeOpen :: Name.Name -> Src.Exposing
typeOpen name =
  Src.Explicit [ A.At R.zero (Src.Upper name Src.Public) ]


typeClosed :: Name.Name -> Src.Exposing
typeClosed name =
  Src.Explicit [ A.At R.zero (Src.Upper name Src.Private) ]


operator :: Name.Name -> Src.Exposing
operator op =
  Src.Explicit [ A.At R.zero (Src.Operator op) ]
