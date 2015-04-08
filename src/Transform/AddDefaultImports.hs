module Transform.AddDefaultImports (add, defaultImports) where

import qualified AST.Module as Module
import qualified AST.Variable as Var


-- DESCRIPTION OF DEFAULT IMPORTS

(==>) :: a -> b -> (a,b)
(==>) = (,)


defaultImports :: [(Module.Name, Module.ImportMethod)]
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

add :: Bool -> Module.Module exs body -> Module.Module exs body
add needsDefaults (Module.Module moduleName path exports imports decls) =
    Module.Module moduleName path exports ammendedImports decls
  where
    ammendedImports =
      if needsDefaults
        then defaultImports ++ imports
        else imports
