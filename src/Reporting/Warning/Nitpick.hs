module Reporting.Warning.Nitpick where

import qualified AST.Pattern as Pattern
import qualified AST.Literal as Literal
import qualified Data.Set as Set
import qualified AST.Variable as Var


data Pat var
    = Data var [Pat var]
    | Record [String]
    | Alias String (Pat var)
    | Var String
    | Anything
    | Literal Literal.Literal
    -- It's a shame that I had to replicate the entire pattern type, but it's
    -- essential to have this constructor.
    | AnythingBut (Set.Set Literal.Literal)
    deriving (Show, Eq)


type CanonicalPat =
    Pat Var.Canonical


data Warning
    = Redundant Pattern.CanonicalPattern
    -- The argument is the list of patterns that would have to
    -- be added for the case expression to be exhaustive.
    | Inexhaustive [CanonicalPat]

