module Transform.AddDefaultImports (add, defaultImports) where

import qualified AST.Module as Module
import qualified AST.Variable as Var


-- DESCRIPTION OF DEFAULT IMPORTS

(==>) :: a -> b -> (a,b)
(==>) = (,)


defaultImports :: [Module.DefaultImport]
defaultImports =
    [ ["Basics"] ==> Module.ImportMethod Nothing Var.openListing
    , ["List"] ==> exposing [Var.Value "::"]
    , ["Maybe"] ==> exposing [Var.Union "Maybe" Var.openListing]
    , ["Result"] ==> exposing [Var.Union "Result" Var.openListing]
    , ["Signal"] ==> exposing [Var.Alias "Signal"]
    ]


exposing :: [Var.Value] -> Module.ImportMethod
exposing vars =
  Module.ImportMethod Nothing (Var.listing vars)


-- ADDING DEFAULT IMPORTS TO A MODULE

add
    :: Bool
    -> [Module.UserImport]
    -> ([Module.DefaultImport], [Module.UserImport])
add needsDefaults imports =
    (defaults, imports)
  where
    defaults =
      if needsDefaults
        then defaultImports
        else []
