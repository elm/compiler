module Reporting.Warning where

import qualified AST.Module as Module
import qualified AST.Type as Type


data Warning
    = UnusedImport Module.Name
    | MissingTypeAnnotation String Type.Canonical
