module Rename (rename) where

import Ast
import Control.Arrow (first)
import Control.Monad.State
import Data.Char (isLower)
import Data.List (mapAccumL)
import Data.Functor.Identity

rename expr = evalState (rename' id expr) 0

guid :: State Int Int
guid = do n <- get
          put (n+1)
          return n

rename' env expr =
    case expr of
      Range e1 e2 -> do
        re1 <- rename' env e1; re2 <- rename' env e2
        return $ Range re1 re2
      Access e x -> do
                re <- rename' env e
                return $ Access re x
      Binop op e1 e2 ->
          do let rop = if isLower (head op) || '_' == head op then env op else op
             re1 <- rename' env e1; re2 <- rename' env e2
             return $ Binop rop re1 re2
      Lambda x e -> do (rx, env') <- extend env x
                       rename' env' e >>= return . Lambda rx
      App e1 e2 -> do
        re1 <- rename' env e1; re2 <- rename' env e2
        return $ App re1 re2
      If e1 e2 e3 -> do
        re1 <- rename' env e1; re2 <- rename' env e2; re3 <- rename' env e3
        return $ If re1 re2 re3
      Lift e es -> do
        re <- rename' env e
        mapM (rename' env) es >>= return . Lift re
      Fold e1 e2 e3 -> do
        re1 <- rename' env e1; re2 <- rename' env e2; re3 <- rename' env e3
        return $ Fold re1 re2 re3
      Async e -> rename' env e >>= return . Async
      Let defs e -> do
                let (vs,es) = unzip defs
                env' <- foldM (\acc x -> extend acc x >>= return . snd) env vs
                es' <- mapM (rename' env') es; re <- rename' env' e
                return $ Let (zip (map env' vs) es') re
      Var x -> return . Var $ env x
      Data name es -> mapM (rename' env) es >>= return . Data name
      Case e cases -> do
                re <- rename' env e
                mapM (pattern_rename env) cases >>= return . (Case re)
      _ -> return expr

extend env x = do
  n <- guid
  let rx = x ++ "_" ++ show n
  return (rx, (\y -> if y == x then rx else env y))

pattern_extend pattern env =
    case pattern of
      PAnything -> return (PAnything, env)
      PVar x -> extend env x >>= return . first PVar
      PData name ps ->
          foldM f ([],env) ps >>= return . first (PData name . reverse)
                 where f (rps,env') p = do (rp,env'') <- pattern_extend p env'
                                           return (rp:rps, env'')

pattern_rename env (p,e) = do
  (rp,env') <- pattern_extend p env
  re <- rename' env' e
  return (rp,re)