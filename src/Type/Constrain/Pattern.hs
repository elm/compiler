{-# LANGUAGE FlexibleInstances #-}
module Type.Constrain.Pattern where

import Control.Arrow (second)
import Control.Applicative ((<$>))
import qualified Control.Monad as Monad
import qualified Data.Map as Map

import qualified AST.Pattern as P
import qualified AST.Variable as V
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Type.Constrain.Literal as Literal
import qualified Type.Environment as Env
import Type.Fragment
import Type.Type


constrain
    :: Env.Environment
    -> P.CanonicalPattern
    -> Type
    -> IO Fragment
constrain env (A.A region pattern) tipe =
  let
    (===) = CEqual Error.None region

    rvar v =
      A.A region (varN v)
  in
  case pattern of
    P.Anything ->
        return emptyFragment

    P.Literal lit ->
        do  c <- Literal.constrain env region lit tipe
            return $ emptyFragment { typeConstraint = c }

    P.Var name ->
        do  v <- variable Flexible
            return $ Fragment
                { typeEnv = Map.singleton name (rvar v)
                , vars = [v]
                , typeConstraint = varN v === tipe
                }

    P.Alias name p ->
        do  v <- variable Flexible
            fragment <- constrain env p tipe
            return $ fragment
              { typeEnv = Map.insert name (rvar v) (typeEnv fragment)
              , vars = v : vars fragment
              , typeConstraint = varN v === tipe /\ typeConstraint fragment
              }

    P.Data name patterns ->
        do  (_kind, cvars, args, result) <-
                Env.freshDataScheme env (V.toString name)

            fragList <- Monad.zipWithM (constrain env) patterns args
            let fragment = joinFragments fragList
            return $ fragment
                { typeConstraint = typeConstraint fragment /\ tipe === result
                , vars = cvars ++ vars fragment
                }

    P.Record fields ->
        do  pairs <- mapM (\name -> (,) name <$> variable Flexible) fields
            let tenv = Map.fromList (map (second rvar) pairs)
            c <- exists $ \t -> return (tipe === record (Map.map (\v -> [A.drop v]) tenv) t)
            return $ Fragment
                { typeEnv = tenv
                , vars = map snd pairs
                , typeConstraint = c
                }
