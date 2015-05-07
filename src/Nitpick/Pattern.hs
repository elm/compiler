module Nitpick.Pattern where

import qualified AST.Pattern as P
import qualified AST.Variable as Var


data Pattern
    = Data Var.Canonical [Pattern]
    | Record [String]
    | Alias String Pattern
    | Var String
    | Anything
    | Literal Literal.Literal
    | AnythingBut (Set.Set Literal.Literal)
    deriving (Show, Eq)


fromPattern :: P.CanonicalPattern -> Pattern
fromPattern (Annotation.A _region pattern) =
    case pattern of
      P.Data ctor patterns ->
          Data ctor (map fromPattern patterns)

      P.Record fields ->
          Record fields

      P.Alias name pattern ->
          Alias name (fromPattern pattern)

      P.Var name ->
          Var name

      P.Anything ->
          Anything

      P.Literal literal ->
          Literal literal

