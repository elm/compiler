module Elm.Compiler.Imports (defaults) where

import qualified AST.Module as Module
import qualified AST.Variable as Var


-- DEFAULT IMPORTS

(==>) :: a -> b -> (a,b)
(==>) = (,)


defaults :: [Module.DefaultImport]
defaults =
    [ ["Basics"] ==> Module.ImportMethod Nothing Var.openListing
    , ["List"] ==> exposing [Var.Value "::"]
    , ["Maybe"] ==> exposing [Var.Union "Maybe" Var.openListing]
    , ["Result"] ==> exposing [Var.Union "Result" Var.openListing]
    , ["Signal"] ==> exposing [Var.Alias "Signal"]
    ]


exposing :: [Var.Value] -> Module.ImportMethod
exposing vars =
  Module.ImportMethod Nothing (Var.listing vars)
