{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rename (renameModule, derename, deprime) where

import Ast
import Context
import Control.Arrow (first)
import Control.Monad (ap, liftM, foldM, mapM, Monad, zipWithM)
import Control.Monad.State (evalState, State, get, put)
import Data.Char (isLower,isDigit)
import Guid

derename var
    | isDigit (last var) = reverse . tail . dropWhile isDigit $ reverse var
    | otherwise = var

renameModule :: Module -> Module
renameModule modul = run (rename deprime modul)

class Rename a where
  rename :: (String -> String) -> a -> GuidCounter a

instance Rename Module where 
  rename env (Module name ex im stmts) = do stmts' <- renameStmts env stmts
                                            return (Module name ex im stmts')

instance Rename Def where
  rename env (OpDef op a1 a2 e) =
      do env' <- extends env [a1,a2]
         OpDef op (env' a1) (env' a2) `liftM` rename env' e
  rename env (FnDef f args e) =
      do env' <- extends env args
         FnDef (env f) (map env' args) `liftM` rename env' e

instance Rename Statement where
  rename env (Definition def) = Definition `liftM` rename env def
  rename env (Datatype name args tcs) =
      return $ Datatype name args $ map (first env) tcs
  rename env (ImportEvent js base elm tipe) =
      do base' <- rename env base
         return $ ImportEvent js base' (env elm) tipe
  rename env (ExportEvent js elm tipe) =
      return $ ExportEvent js (env elm) tipe

renameStmts env stmts = do env' <- extends env $ concatMap getNames stmts
                           mapM (rename env') stmts
    where getNames stmt = case stmt of
                            Definition (FnDef n _ _) -> [n]
                            Datatype _ _ tcs -> map fst tcs
                            ImportEvent _ _ n _ -> [n]
                            _ -> []

instance Rename a => Rename (Context a) where
  rename env (C t s e) = C t s `liftM` rename env e
                          
instance Rename Expr where
  rename env expr =
    let rnm = rename env in
    case expr of

      Range e1 e2 -> Range `liftM` rnm e1
                              `ap` rnm e2
      
      Access e x -> Access `liftM` rnm e
                              `ap` return x

      Modify e f v  -> flip Modify f `liftM` rnm e
                                        `ap` rnm v

      Record fs -> Record `liftM` mapM frnm fs
          where frnm (f,as,e) = do env' <- extends env as
                                   e' <- rename env' e
                                   return (f, map env' as, e') 

      Binop op@(h:_) e1 e2 ->
        let rop = if isLower h || '_' == h
                  then env op
                  else op
        in Binop rop `liftM` rnm e1
                        `ap` rnm e2

      Lambda x e -> do
          (rx, env') <- extend env x
          Lambda rx `liftM` rename env' e

      App e1 e2 -> App `liftM` rnm e1
                          `ap` rnm e2

      If e1 e2 e3 -> If `liftM` rnm e1
                           `ap` rnm e2
                           `ap` rnm e3

      MultiIf ps -> MultiIf `liftM` mapM grnm ps
              where grnm (b,e) = (,) `liftM` rnm b
                                        `ap` rnm e

      Let defs e -> renameLet env defs e

      Var x -> return . Var $ env x

      Data name es -> Data name `liftM` mapM rnm es

      Case e cases -> Case `liftM` rnm e
                              `ap` mapM (patternRename env) cases

      _ -> return expr

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

patternRename :: (String -> String) -> (Pattern, CExpr) -> GuidCounter (Pattern, CExpr)
patternRename env (p,e) = do
  (rp,env') <- patternExtend p env
  re <- rename env' e
  return (rp,re)

renameLet env defs e = do env' <- extends env $ concatMap getNames defs
                          defs' <- mapM (rename env') defs
                          Let defs' `liftM` rename env' e
    where getNames (FnDef n _ _)   = [n]
          getNames (OpDef _ _ _ _) = []