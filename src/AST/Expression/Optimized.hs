{-# OPTIONS_GHC -Wall #-}
module AST.Expression.Optimized
    ( Def(..), Facts(..), dummyFacts
    , Expr(..)
    , Decider(..), Choice(..)
    ) where

import Data.Text (Text)

import qualified AST.Expression.Canonical as Can
import qualified AST.Literal as Literal
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Optimize.DecisionTree as DT
import qualified Reporting.Region as R



-- DEFINITIONS

data Def
    = Def Facts Text Expr
    | TailDef Facts Text [Text] Expr


data Facts = Facts
    { home :: Maybe ModuleName.Canonical
    }


dummyFacts :: Facts
dummyFacts =
  Facts Nothing


-- EXPRESSIONS

data Expr
    = Literal Literal.Literal
    | Var Var.Canonical
    | List [Expr]
    | Binop Var.Canonical Expr Expr
    | Function [Text] Expr
    | Call Expr [Expr]
    | TailCall Text [Text] [Expr]
    | If [(Expr, Expr)] Expr
    | Let [Def] Expr
    | Case Text (Decider Choice) [(Int, Expr)]
    | Ctor Text [Expr]
    | CtorAccess Expr Int
    | Access Expr Text
    | Update Expr [(Text, Expr)]
    | Record [(Text, Expr)]
    | Cmd ModuleName.Canonical
    | Sub ModuleName.Canonical
    | OutgoingPort Text Type.Canonical
    | IncomingPort Text Type.Canonical
    | Program Can.Main Expr
    | GLShader Text Text Literal.GLShaderTipe
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
