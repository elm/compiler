module Type.Constrain.Pattern where

import Control.Arrow (second)
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
    equal patternError leftType rightType =
      CEqual (Error.Pattern patternError) region leftType rightType

    rvar v =
      A.A region (VarN v)
  in
  case pattern of
    P.Anything ->
        return emptyFragment

    P.Literal lit ->
        do  c <- Literal.constrain env region lit tipe
            return $ emptyFragment { typeConstraint = c }

    P.Var name ->
        do  variable <- mkVar Nothing
            return $ Fragment
                { typeEnv = Map.singleton name (rvar variable)
                , vars = [variable]
                , typeConstraint =
                    equal (Error.PVar name) (VarN variable) tipe
                }

    P.Alias name p ->
        do  variable <- mkVar Nothing
            fragment <- constrain env p tipe
            return $ fragment
              { typeEnv = Map.insert name (rvar variable) (typeEnv fragment)
              , vars = variable : vars fragment
              , typeConstraint =
                  equal (Error.PAlias name) (VarN variable) tipe
                  /\ typeConstraint fragment
              }

    P.Data name patterns ->
        do  let stringName = V.toString name

            (_kind, cvars, args, result) <-
                Env.freshDataScheme env stringName

            fragList <- Monad.zipWithM (constrain env) patterns args
            let fragment = joinFragments fragList
            return $ fragment
                { vars = cvars ++ vars fragment
                , typeConstraint =
                    typeConstraint fragment
                    /\ equal (Error.PData stringName) tipe result
                }

    P.Record fields ->
        do  pairs <-
                mapM (\name -> (,) name <$> mkVar Nothing) fields

            let tenv =
                  Map.fromList (map (second rvar) pairs)

            let unannotatedTenv =
                  Map.map A.drop tenv

            con <- exists $ \t ->
              return (equal Error.PRecord tipe (record unannotatedTenv t))

            return $ Fragment
                { typeEnv = tenv
                , vars = map snd pairs
                , typeConstraint = con
                }
