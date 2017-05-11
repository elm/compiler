{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler.Imports (defaults) where

import Data.Text (Text)

import qualified AST.Exposing as E
import qualified AST.Module as Module
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- DEFAULT IMPORTS


(==>) :: a -> b -> (a,b)
(==>) = (,)


defaults :: [Module.DefaultImport]
defaults =
    [ "Basics" ==> Module.ImportMethod Nothing E.Open
    , "Debug" ==> Module.ImportMethod Nothing E.closed
    , "List" ==> exposing [E.Lower "::"]
    , "Maybe" ==> exposing [E.Upper "Maybe" (Just E.Open)]
    , "Result" ==> exposing [E.Upper "Result" (Just E.Open)]
    , "String" ==> Module.ImportMethod Nothing E.closed
    , "Tuple" ==> exposing [E.Lower "=>"]
    , "Platform" ==> exposing [closedType "Program"]
    , "Platform.Cmd" ==> named "Cmd" [closedType "Cmd"]
    , "Platform.Sub" ==> named "Sub" [closedType "Sub"]
    ]


exposing :: [E.Entry] -> Module.ImportMethod
exposing entries =
  Module.ImportMethod Nothing (explicit entries)


closedType :: Text -> E.Entry
closedType name =
  E.Upper name Nothing


named :: Text -> [E.Entry] -> Module.ImportMethod
named name entries =
  Module.ImportMethod (Just name) (explicit entries)


explicit :: [E.Entry] -> E.Raw
explicit entries =
  E.Explicit (map (A.A nowhere) entries)


nowhere :: R.Region
nowhere =
  R.Region (R.Position 1 1) (R.Position 1 1)
