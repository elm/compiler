{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rename (rename) where

import Ast
import Control.Monad (ap, liftM, mapM, Monad)
import Control.Monad.State (evalState, State, get, put)
import Data.Char (isLower)
import Data.Maybe (fromMaybe)

data Env = Env { guidCount :: Int
               , environment :: [(String, String)] }
    deriving (Show)

newtype Environment a = E { runE :: State Env a }
    deriving (Monad)

-- Generate a new name for x.
envExtend :: String -> Environment String
envExtend x = E $ do env <- get
                     let guid = guidCount env
                         e    = environment env
                         newX = x ++ "_" ++ show guid
                     put $ env { guidCount = guid + 1
                               , environment = (x, newX):e }
                     return newX

-- Get the name to use for x. If x was renamed, the new name will be used,
-- otherwise, the oringal name.
envLookup :: String -> Environment String
envLookup x = E $ get >>= return . fromMaybe x . lookup x . environment


rename :: Expr -> Expr
rename expr = evalState (runE $ rename' expr) $ Env { guidCount = 0
                                                    , environment = [] }


rename' :: Expr -> Environment Expr

rename' (Range e1 e2) = Range `liftM` rename' e1
                                 `ap` rename' e1

rename' (Access e x) = Access `liftM` rename' e
                                 `ap` return x

rename' (Binop op e1 e2) = Binop `liftM` resolveOp op
                                    `ap` rename' e1
                                    `ap` rename' e2
    where resolveOp op@(h:_)
            | isLower h || '_' == h = envLookup op
            | otherwise             = return op

rename' (Lambda x e) = Lambda `liftM` envExtend x
                                 `ap` rename' e

rename' (App e1 e2) = App `liftM` rename' e1
                             `ap` rename' e2

rename' (If e1 e2 e3) = If `liftM` rename' e1
                              `ap` rename' e2
                              `ap` rename' e3

rename' (Lift e es) = Lift `liftM` rename' e
                              `ap` mapM rename' es

rename' (Fold e1 e2 e3) = Fold `liftM` rename' e1
                                  `ap` rename' e2
                                  `ap` rename' e3

rename' (Async e) = Async `liftM` rename' e

rename' (Let defs e) = Let `liftM` mapM letF defs
                              `ap` rename' e
    where letF (x, exp) = do x'   <- envExtend x
                             exp' <- rename' exp
                             return (x', exp')

rename' (Var x) = Var `liftM` envLookup x

rename' (Data name es) = Data name `liftM` mapM rename' es

rename' (Case e cases) = Case `liftM` rename' e
                                 `ap` mapM patternRename cases

rename' e = return e


patternExtend :: Pattern -> Environment Pattern
patternExtend PAnything = return PAnything
patternExtend (PVar x)  = PVar `liftM` envExtend x
patternExtend (PData name ps) = PData name `liftM` mapM patternExtend ps

patternRename :: (Pattern, Expr) -> Environment (Pattern, Expr)
patternRename (p, e) = do newP <- patternExtend p
                          newE <- rename' e
                          return (p, e)
