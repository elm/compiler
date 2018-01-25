{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler.Imports
  ( addDefaults
  )
  where


import qualified AST.Source as Src
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
  [ import_ "Basics" Nothing Src.Open
  , import_ "Debug" Nothing closed
  , import_ "List" Nothing (operator "::")
  , import_ "Maybe" Nothing (typeOpen "Maybe")
  , import_ "Result" Nothing (typeOpen "Result")
  , import_ "String" Nothing (typeClosed "String")
  , import_ "Char" Nothing (typeClosed "Char")
  , import_ "Tuple" Nothing closed
  , import_ "Platform" Nothing (typeClosed "Program")
  , import_ "Platform.Cmd" (Just "Cmd") (typeClosed "Cmd")
  , import_ "Platform.Sub" (Just "Sub") (typeClosed "Sub")
  ]


import_ :: N.Name -> Maybe N.Name -> Src.Exposing -> Src.Import
import_ name maybeAlias exposing =
  Src.Import (A.At zero name) maybeAlias exposing


zero :: R.Region
zero =
  R.Region (R.Position 0 0) (R.Position 0 0)



-- EXPOSING


closed :: Src.Exposing
closed =
  Src.Explicit []


typeOpen :: N.Name -> Src.Exposing
typeOpen name =
  Src.Explicit [ A.At zero (Src.Upper name Src.Public) ]


typeClosed :: N.Name -> Src.Exposing
typeClosed name =
  Src.Explicit [ A.At zero (Src.Upper name Src.Private) ]


operator :: N.Name -> Src.Exposing
operator op =
  Src.Explicit [ A.At zero (Src.Operator op) ]
