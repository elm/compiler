module Reporting.Warning where

import qualified AST.Module as Module


data Warning
    = UnusedImport Module.Name
    | MissingTypeAnnotation String