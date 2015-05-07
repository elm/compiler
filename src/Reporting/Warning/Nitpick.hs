module Reporting.Warning.Nitpick where

import qualified AST.Pattern as Pattern
import qualified AST.Literal as Literal
import qualified Data.Set as Set
import qualified AST.Variable as Var


data Warning
    = Redundant Pattern.CanonicalPattern
    -- The argument is the list of patterns that would have to
    -- be added for the case expression to be exhaustive.
    | Inexhaustive [CanonicalPat]

