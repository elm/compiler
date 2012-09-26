{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rename (rename, derename) where

import Ast
import Control.Arrow (first)
import Control.Monad (ap, liftM, foldM, mapM, Monad, zipWithM)
import Control.Monad.State (evalState, State, get, put)
import Data.Char (isLower,isDigit)
import Guid

derename var
    | isDigit (last var) = reverse . tail . dropWhile isDigit $ reverse var
    | otherwise = var

rename :: Module -> Module
rename (Module name ex im stmts) =
    Module name ex im . run $ renameStatements deprime stmts

renameStatements env stmts = do env' <- extends env $ concatMap getNames stmts
                                mapM (renameStmt env') stmts
    where getNames stmt = case stmt of Def n _ _ -> [n]
                                       Datatype _ _ tcs -> map fst tcs
                                       ImportEvent _ _ n _ -> [n]
                                       ExportEvent _ _ _ -> []
          renameStmt env (Def name args e) =
              do env' <- extends env args
                 Def (env name) (map env' args) `liftM` rename' env' e
          renameStmt env (Datatype name args tcs) =
              return $ Datatype name args $ map (first env) tcs
          renameStmt env (ImportEvent js base elm tipe) =
              do base' <- rename' env base
                 return $ ImportEvent js base' (env elm) tipe
          renameStmt env (ExportEvent js elm tipe) =
                 return $ ExportEvent js (env elm) tipe

rename' :: (String -> String) -> Expr -> GuidCounter Expr
rename' env expr =
    case expr of

      Range e1 e2 -> Range `liftM` rnm e1
                              `ap` rnm e2
      
      Access e x -> Access `liftM` rnm e
                              `ap` return x

      Binop op@(h:_) e1 e2 ->
        let rop = if isLower h || '_' == h
                  then env op
                  else op
        in Binop rop `liftM` rnm e1
                        `ap` rnm e2

      Lambda x e -> do
          (rx, env') <- extend env x
          Lambda rx `liftM` rename' env' e

      App e1 e2 -> App `liftM` rnm e1
                          `ap` rnm e2

      If e1 e2 e3 -> If `liftM` rnm e1
                           `ap` rnm e2
                           `ap` rnm e3

      Lift e es -> Lift `liftM` rnm e
                           `ap` mapM rnm es

      Fold e1 e2 e3 -> Fold `liftM` rnm e1
                               `ap` rnm e2
                               `ap` rnm e3

      Async e -> Async `liftM` rnm e

      Let defs e -> renameLet env defs e

      Var x -> return . Var $ env x

      Data name es -> Data name `liftM` mapM rnm es

      Case e cases -> Case `liftM` rnm e
                              `ap` mapM (patternRename env) cases

      _ -> return expr

  where rnm = rename' env

deprime = map (\c -> if c == '\'' then '_' else c)

extend :: (String -> String) -> String -> GuidCounter (String, String -> String)
extend env x = do
  n <- guid
  let rx = deprime x ++ "_" ++ show n
  return (rx, \y -> if y == x then rx else env y)

extends :: (String -> String) -> [String] -> GuidCounter (String -> String)
extends env xs = foldM (\e x -> liftM snd $ extend e x) env xs

patternExtend :: Pattern -> (String -> String) -> GuidCounter (Pattern, String -> String)
patternExtend pattern env =
    case pattern of
      PAnything -> return (PAnything, env)
      PVar x -> first PVar `liftM` extend env x
      PData name ps ->
          first (PData name . reverse) `liftM` foldM f ([], env) ps
                 where f (rps,env') p = do (rp,env'') <- patternExtend p env'
                                           return (rp:rps, env'')

patternRename :: (String -> String) -> (Pattern, Expr) -> GuidCounter (Pattern, Expr)
patternRename env (p,e) = do
  (rp,env') <- patternExtend p env
  re <- rename' env' e
  return (rp,re)

renameLet env defs e = do env' <- extends env $ map getNames defs
                          defs' <- mapM (renameDef env') defs
                          Let defs' `liftM` rename' env' e
    where getNames (Definition n _ _) = n
          renameDef env (Definition name args e) =
              do env' <- extends env args
                 Definition (env name) (map env' args) `liftM` rename' env' e
