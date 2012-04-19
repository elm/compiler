
module TypeCheck where

import Ast
import Control.Monad
import Control.Monad.Trans.Error
import Data.IORef
import Data.List (foldl', foldl1')
import qualified Data.Map as Map
import System.IO.Unsafe
import TypeHints
import Types

{-- -- Testing Code
import Testing
typeCheck' s = str2expr s >>= typeCheck
--}
{--
instance Monad (Either e) where
        return = Right
        Right m >>= k = k m
        Left e  >>= _ = Left e
--}
typeCheck prog = unsafePerformIO $ do
                   tipe <- tc typeHints prog
                   case tipe of
                     Right t -> unify (ADT "Element") t
                     Left _ -> return tipe

guess = liftM GuessT (newIORef Nothing)

varType env x =
    case Map.lookup x env of
      Just t -> return $ Right t
      Nothing -> liftM Right $ guess
      -- return . Left $ "Variable '" ++ x ++ "' is not bound to a value!"
      -- This is no good because some fundamental functions are only
      -- defined in JS, but it is the right thing to do.

tipe = return . Right

tc :: Map.Map String Type -> Exp -> IO (Either String Type)
tc env exp =
    case exp of
      Number _ -> tipe NumberT
      Var x -> varType env x
      Str _ -> tipe StringT
      Chr _ -> tipe CharT
      Boolean _ -> tipe BoolT
      List es -> do
               either_ts <- liftM sequence $ mapM (tc env) es
               case either_ts of
                 Right [] -> liftM (Right . ListT) guess
                 Right (t:ts) ->
                     return . liftM ListT =<< foldl' func (return $ Right t) ts
                         where func iet t = do
                                 et <- iet
                                 case et of { Right t' -> unify t t'
                                            ; _ -> return et }
                 Left err -> return $ Left err
      Range start end -> return . liftM ListT =<< unifyWith env start end unify
      Tuple es -> return . liftM TupleT . sequence =<< mapM (tc env) es
{--
      Record r -> do
               rList <- mapM (\(x,v) -> tc env v >>= tipe . (,) x) $
                        Map.toList r
               tipe . RecordT $ Map.fromList rList
      Access e x -> do
               t <- tc env e
               let err = error $ "Field '" ++ x ++
                         "' does not exist in record '" ++ show t ++ "'."
               case t of
                 RecordT tRec -> tipe $ Map.findWithDefault err x tRec
                 GuessT _ -> guess
                 _ -> error $ "Expecting a record, but saw '" ++
                      show t ++ "' instead."
--}
      Binop op e1 e2 -> unifyWith env e1 e2 (checkBinop op)
      Lambda x e -> do g <- guess
                       return . liftM (LambdaT g) =<< tc (Map.insert x g env) e
      App e1 e2 -> do
               g <- guess
               unifyWith env e1 e2 $
                   \ta tb -> do
                     either_lam <- unify ta (LambdaT tb g)
                     case either_lam of { Right t -> return $ Right g
                                        ; Left _ -> return either_lam }
      Let x e1 e2 -> tc env (App (Lambda x e2) e1)
      If eb et ef -> do
        either_tb <- unifyWith env eb (Boolean True) unify
        case either_tb of
          Right _ -> unifyWith env et ef unify
          Left _ -> return either_tb
      _ -> return . Right =<< guess
      -- return . Left $ "Failure during type-checking: not implemented yet"

unifyWith env e1 e2 f = do
  t1 <- tc env e1
  t2 <- tc env e2
  case (t1, t2) of { (Right ta, Right tb) -> f ta tb
                   ; (Left _, _) -> return t1
                   ; (_, Left _) -> return t2 }


check es = return . foldl1' f =<< sequence es
    where f b e = case b of { Right _ -> e; Left _ -> b }

checkBinop Cons t1 t2 = unify (ListT t1) t2
checkBinop Append t1 t2 = check [guess >>= unify t1 . ListT, unify t1 t2]
checkBinop Compose t1 t2 = do
  ga <- guess; gb <- guess; gc <- guess
  check [ unify t2 (LambdaT ga gb)
        , unify t1 (LambdaT gb gc)
        , tipe (LambdaT ga gc) ]
checkBinop Apply t1 t2 = do { g <- guess
                            ; check [unify t1 (LambdaT t2 g), tipe g] }
checkBinop op t1 t2
    | elem op [And, Or] = check [unify BoolT t1, unify BoolT t2]
    | elem op [Equ, Neq] = check [guess >>= unify t1, unify t1 t2, tipe BoolT]
    | elem op [Lt, Lte, Gt, Gte] =
        check [unify NumberT t1, unify NumberT t2, tipe BoolT]
    | elem op [Add, Sub, Mul, Div] = check [unify NumberT t1, unify NumberT t2]

unify t1 t2
    | t1 == t2 = tipe t1
    | otherwise =
        case (t1,t2) of
          (StringT, _) -> unify (ListT CharT) t2
          (_, StringT) -> unify t1 (ListT CharT)
          (ListT t1', ListT t2') -> return . fmap ListT =<< unify t1' t2'
          (TupleT ts1, TupleT ts2) ->
              return . fmap TupleT . sequence =<< zipWithM unify ts1 ts2
{--
          (RecordT tMap1, RecordT tMap2) ->
              tipe . RecordT =<< unifyTmap tMap1 tMap2
--}
          (GuessT ref, _) -> 
              if t2 `contains` ref then
                  return . Left $ "Cannot construct infinite type: " ++
                         show t1 ++ " = " ++ show t2
              else do
                g <- readIORef ref
                case g of
                  Just t1' -> unify t1' t2
                  Nothing -> writeIORef ref (Just t2) >> tipe t1
          (_, GuessT ref) ->  -- unify t2 t1
              if t1 `contains` ref then
                  return . Left $ "Cannot construct infinite type: " ++
                         show t1 ++ " = " ++ show t2
              else do
                g <- readIORef ref
                case g of
                  Just t2' -> unify t1 t2'
                  Nothing -> writeIORef ref (Just t1) >> tipe t2
          (LambdaT t1a t1b, LambdaT t2a t2b) -> do
            ta <- unify t1a t2a
            tb <- unify t1b t2b
            return $ liftM2 LambdaT ta tb
          (_, _) -> return . Left $
                   "Type Error: saw type '" ++ show t2 ++
                   "' but expecting type '" ++ show t1 ++ "'"

unifyTmap tMap1 tMap2
    | Map.keysSet tMap1 /= Map.keysSet tMap2 =
        error "record unification failed"
    | otherwise = Map.intersectionWith unify tMap1 tMap2

t `contains` ref =
  case t of
    ListT t' -> t' `contains` ref
    TupleT ts -> any (`contains` ref) ts
    LambdaT t1 t2 -> t1 `contains` ref || t2 `contains` ref
    {- RecordT (Map.Map String Type)-}
    GuessT ref' -> ref == ref'
    _ -> False
