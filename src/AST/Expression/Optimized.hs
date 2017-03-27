{-# OPTIONS_GHC -Wall #-}
module AST.Expression.Optimized
  ( Decl(..)
  , Def(..)
  , Expr(..)
  , Decider(..), Choice(..)
  )
  where


import qualified Data.Set as Set
import Data.Text (Text)

import qualified AST.Effects as Effects
import qualified AST.Expression.Canonical as Can
import qualified AST.Literal as Literal
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Optimize.DecisionTree as DT
import qualified Reporting.Region as R



-- TOP LEVEL DECLARATIONS


data Decl =
  Decl
    { _deps :: Set.Set Var.Global
    , _effects :: Maybe Effects.ManagerType
    , _body :: !Def
    }



-- DEFINITIONS


data Def
  = Def Expr
  | TailDef [Text] Expr



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
    | Let [(Text, Def)] Expr
    | Case Text (Decider Choice) [(Int, Expr)]
    | Ctor Text [Expr]
    | CtorAccess Expr Int
    | Access Expr Text
    | Update Expr [(Text, Expr)]
    | Record [(Text, Expr)]
    | Cmd ModuleName.Canonical Effects.ManagerType
    | Sub ModuleName.Canonical Effects.ManagerType
    | OutgoingPort Text Type.Canonical
    | IncomingPort Text Type.Canonical
    | Program Can.Main Expr
    | GLShader Text Text Literal.Shader
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
