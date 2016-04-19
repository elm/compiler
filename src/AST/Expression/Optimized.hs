{-# OPTIONS_GHC -Wall #-}
module AST.Expression.Optimized
    ( Def(..), Facts(..), dummyFacts
    , Expr(..)
    , Decider(..), Choice(..)
    ) where

import qualified AST.Expression.General as Expr
import qualified AST.Literal as Literal
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Optimize.DecisionTree as DT
import qualified Reporting.Region as R



-- DEFINITIONS

data Def
    = Def Facts String Expr
    | TailDef Facts String [String] Expr


data Facts = Facts
    { home :: Maybe ModuleName.Canonical
    , dependencies :: [Var.TopLevel]
    }


dummyFacts :: Facts
dummyFacts =
  Facts Nothing []


-- EXPRESSIONS

data Expr
    = Literal Literal.Literal
    | Var Var.Canonical
    | Range Expr Expr
    | ExplicitList [Expr]
    | Binop Var.Canonical Expr Expr
    | Function [String] Expr
    | Call Expr [Expr]
    | TailCall String [String] [Expr]
    | If [(Expr, Expr)] Expr
    | Let [Def] Expr
    | Case String (Decider Choice) [(Int, Expr)]
    | Data String [Expr]
    | DataAccess Expr Int
    | Access Expr String
    | Update Expr [(String, Expr)]
    | Record [(String, Expr)]
    | Cmd ModuleName.Canonical
    | Sub ModuleName.Canonical
    | OutgoingPort String Type.Canonical
    | IncomingPort String Type.Canonical
    | Program (Expr.Main Type.Canonical) Expr
    | GLShader String String Literal.GLShaderTipe
    | Crash ModuleName.Canonical R.Region (Maybe Expr)


data Decider a
    = Leaf a
    | Chain
        { _testChain :: [(DT.Path, DT.Test)]
        , _success :: Decider a
        , _failure :: Decider a
        }
    | FanOut
        { _path :: DT.Path
        , _tests :: [(DT.Test, Decider a)]
        , _fallback :: Decider a
        }
    deriving (Eq)


data Choice
    = Inline Expr
    | Jump Int
