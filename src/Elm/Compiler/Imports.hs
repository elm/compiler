module Elm.Compiler.Imports (defaults) where

import qualified AST.Module as Module
import qualified AST.Variable as Var


-- DEFAULT IMPORTS

(==>) :: a -> b -> (a,b)
(==>) = (,)


defaults :: [Module.DefaultImport]
defaults =
    [ ["Basics"] ==> Module.ImportMethod Nothing Var.openListing
    , ["Debug"] ==> Module.ImportMethod Nothing Var.closedListing
    , ["List"] ==> exposing [Var.Value "::"]
    , ["Maybe"] ==> exposing [Var.Union "Maybe" Var.openListing]
    , ["Result"] ==> exposing [Var.Union "Result" Var.openListing]
    , ["String"] ==> Module.ImportMethod Nothing Var.closedListing
    , ["Tuple"] ==> Module.ImportMethod Nothing Var.closedListing
    , ["Platform"] ==> exposing [closedType "Program"]
    , ["Platform","Cmd"] ==> named "Cmd" [closedType "Cmd", Var.Value "!"]
    , ["Platform","Sub"] ==> named "Sub" [closedType "Sub"]
    ]


exposing :: [Var.Value] -> Module.ImportMethod
exposing vars =
  Module.ImportMethod Nothing (Var.listing vars)


closedType :: String -> Var.Value
closedType name =
  Var.Union name Var.closedListing


named :: String -> [Var.Value] -> Module.ImportMethod
named name vars =
  Module.ImportMethod (Just name) (Var.listing vars)

