{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Binops ( Op(..), canonicalize, flatten ) where

import Prelude hiding (last)
import qualified Data.Map as Map
import Data.Text (Text)

import AST.Declaration (Assoc(..))
import qualified AST.Expression.Canonical as C
import qualified AST.Variable as Var
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as Region
import qualified Reporting.Result as R



-- CANONICALIZE


data Op =
  Op
    { _name :: Var.Canonical
    , _assoc :: Assoc
    , _prec :: Int
    , _region :: Region.Region
    }


canonicalize :: Env.Env -> A.Located Text -> Var.Result Op
canonicalize env (A.A region op) =
  do  name <- Var.variable region env op
      let (assoc, prec) = Map.findWithDefault (L, 9) name (Env._infixes env)
      return $ Op name assoc prec region



-- FLATTEN


flatten :: (Monoid i) => Region.Region -> [(C.Expr, Op)] -> C.Expr -> R.Result i w Error.Error C.Expr
flatten overallRegion ops last =
  runStepper overallRegion (More ops last)


data Step
  = Done C.Expr
  | More [(C.Expr, Op)] C.Expr
  | Error Op Op


runStepper :: (Monoid i) => Region.Region -> Step -> R.Result i w Error.Error C.Expr
runStepper overallRegion step =
  case step of
    Done expr ->
      R.ok expr

    More [] expr ->
      R.ok expr

    More ( (expr, op@(Op name _ _ _)) : rest ) last ->
      runStepper overallRegion $
        flattenHelp (binop name expr) op rest last

    Error (Op name1 assoc1 prec _) (Op name2 assoc2 _ region) ->
      R.throw overallRegion (Error.BadInfix region prec name1 assoc1 name2 assoc2)


flattenHelp :: (C.Expr -> C.Expr) -> Op -> [(C.Expr, Op)] -> C.Expr -> Step
flattenHelp makeBinop rootOp@(Op _ rootAssoc rootPrec _) middle last =
  case middle of
    [] ->
      Done (makeBinop last)

    ( expr, op@(Op name assoc prec _) ) : rest ->
      if prec < rootPrec then

        More ((makeBinop expr, op) : rest) last

      else if prec > rootPrec then

        case flattenHelp (binop name expr) op rest last of
          Done newLast ->
            Done (makeBinop newLast)

          More newMiddle newLast ->
            flattenHelp makeBinop op newMiddle newLast

          Error a b ->
            Error a b

      else

        case (rootAssoc, assoc) of
          (L, L) ->
            flattenHelp (\right -> binop name (makeBinop expr) right) op rest last

          (R, R) ->
            flattenHelp (\right -> makeBinop (binop name expr right)) op rest last

          (_, _) ->
            Error rootOp op



-- BINOP HELPERS


binop :: Var.Canonical -> C.Expr -> C.Expr -> C.Expr
binop name left right =
  A.merge left right (C.Binop name left right)