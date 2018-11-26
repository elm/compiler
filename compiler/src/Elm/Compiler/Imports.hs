{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler.Imports
  ( addDefaults
  )
  where


import qualified Data.Name as Name

import qualified AST.Source as Src
import qualified Elm.ModuleName as ModuleName
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
  [ import_ ModuleName.basics Nothing Src.Open
  , import_ ModuleName.debug Nothing closed
  , import_ ModuleName.list Nothing (operator "::")
  , import_ ModuleName.maybe Nothing (typeOpen Name.maybe)
  , import_ ModuleName.result Nothing (typeOpen Name.result)
  , import_ ModuleName.string Nothing (typeClosed Name.string)
  , import_ ModuleName.char Nothing (typeClosed Name.char)
  , import_ ModuleName.tuple Nothing closed
  , import_ ModuleName.platform Nothing (typeClosed Name.program)
  , import_ ModuleName.cmd (Just Name.cmd) (typeClosed Name.cmd)
  , import_ ModuleName.sub (Just Name.sub) (typeClosed Name.sub)
  ]


import_ :: ModuleName.Canonical -> Maybe Name.Name -> Src.Exposing -> Src.Import
import_ (ModuleName.Canonical _ name) maybeAlias exposing =
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
