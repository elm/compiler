{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SourceSyntax.Rename (renameModule, derename, deprime) where

import Control.Arrow (first)
import Control.Monad (ap, liftM, foldM, mapM, Monad, zipWithM)
import Control.Monad.State (evalState, State, get, put)
import Data.Char (isLower,isDigit)
import Unique
import SourceSyntax.Location
import SourceSyntax.Pattern
import SourceSyntax.Expression
import SourceSyntax.Declaration hiding (Assoc(..))
import SourceSyntax.Module

derename var
    | isDigit (last var) = reverse . tail . dropWhile isDigit $ reverse var
    | otherwise = var

renameModule :: Module t v -> Module t v
renameModule modul = run (rename deprime modul)

class Rename a where
  rename :: (String -> String) -> a -> Unique a

instance Rename (Module t v) where 
  rename env (Module name ex im stmts) = do stmts' <- renameStmts env stmts
                                            return (Module name ex im stmts')

instance Rename (Def t v) where
  rename env (OpDef op a1 a2 e) =
      do env' <- extends env [a1,a2]
         OpDef op (env' a1) (env' a2) `liftM` rename env' e
  rename env (FnDef f args e) =
      do env' <- extends env args
         FnDef (env f) (map env' args) `liftM` rename env' e
  rename env (TypeAnnotation n t) = return (TypeAnnotation (env n) t)


instance Rename (Declaration t v) where
  rename env stmt =
    case stmt of
      Definition def -> Definition `liftM` rename env def
      Datatype name args tcs ->
          return $ Datatype name args $ map (first env) tcs
      TypeAlias n xs t -> return (TypeAlias n xs t)
      ImportEvent js base elm tipe ->
          do base' <- rename env base
             return $ ImportEvent js base' (env elm) tipe
      ExportEvent js elm tipe ->
          return $ ExportEvent js (env elm) tipe

renameStmts env stmts = do env' <- extends env $ concatMap getNames stmts
                           mapM (rename env') stmts
    where getNames stmt = case stmt of
                            Definition (FnDef n _ _) -> [n]
                            Datatype _ _ tcs -> map fst tcs
                            ImportEvent _ _ n _ -> [n]
                            _ -> []

instance Rename a => Rename (Located a) where
  rename env (L t s e) = L t s `liftM` rename env e
                          
instance Rename (Expr t v) where
  rename env expr =
    let rnm = rename env in
    case expr of

      Range e1 e2 -> Range `liftM` rnm e1
                              `ap` rnm e2
      
      Access e x -> Access `liftM` rnm e
                              `ap` return x

      Remove e x -> flip Remove x `liftM` rnm e

      Insert e x v -> flip Insert x `liftM` rnm e
                                       `ap` rnm v

      Modify e fs  -> Modify `liftM` rnm e
                                `ap` mapM (\(x,e) -> (,) x `liftM` rnm e) fs

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

      MultiIf ps -> MultiIf `liftM` mapM grnm ps
              where grnm (b,e) = (,) `liftM` rnm b
                                        `ap` rnm e

      Let defs e -> renameLet env defs e

      Var x -> return . Var $ env x

      Data name es -> Data name `liftM` mapM rnm es

      Case e cases -> Case `liftM` rnm e
                              `ap` mapM (patternRename env) cases

      _ -> return expr

deprime = map (\c -> if c == '\'' then '$' else c)

extend :: (String -> String) -> String -> Unique (String, String -> String)
extend env x = do
  n <- guid
  let rx = deprime x ++ "_" ++ show n
  return (rx, \y -> if y == x then rx else env y)

extends :: (String -> String) -> [String] -> Unique (String -> String)
extends env xs = foldM (\e x -> liftM snd $ extend e x) env xs

patternExtend :: Pattern -> (String -> String) -> Unique (Pattern, String -> String)
patternExtend pattern env =
    case pattern of
      PAnything -> return (PAnything, env)
      PVar x -> first PVar `liftM` extend env x
      PAlias x p -> do
        (x', env') <- extend env x
        (p', env'') <- patternExtend p env'
        return (PAlias x' p', env'')
      PData name ps ->
          first (PData name . reverse) `liftM` foldM f ([], env) ps
                 where f (rps,env') p = do (rp,env'') <- patternExtend p env'
                                           return (rp:rps, env'')
      PRecord fs ->
          return (pattern, foldr (\f e n -> if n == f then f else env n) env fs)

patternRename :: (String -> String)
              -> (Pattern, LExpr t v)
              -> Unique (Pattern, LExpr t v)
patternRename env (p,e) = do
  (rp,env') <- patternExtend p env
  re <- rename env' e
  return (rp,re)

renameLet env defs e = do env' <- extends env $ concatMap getNames defs
                          defs' <- mapM (rename env') defs
                          Let defs' `liftM` rename env' e
    where getNames (FnDef n _ _)   = [n]
          getNames _ = []
