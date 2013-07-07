module Type.Constrain.Pattern where

import Control.Arrow (second)
import Control.Applicative ((<$>))
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Map ((!))
import qualified Data.Map as Map

import SourceSyntax.Pattern
import Type.Type
import Type.Fragment
import Type.Environment as Env
import qualified Type.Constrain.Literal as Literal


constrain :: Environment -> Pattern -> Type -> IO Fragment
constrain env pattern tipe =
    case pattern of
      PAnything -> return emptyFragment

      PLiteral lit ->
          return $ emptyFragment {
              typeConstraint = Literal.constrain env lit tipe
          }

      PVar name -> do
          v <- flexibleVar
          return $ Fragment {
              typeEnv    = Map.singleton name (VarN v),
              vars       = [v],
              typeConstraint = VarN v === tipe
          }

      PAlias name p -> do
          fragment <- constrain env p tipe
          return $ fragment {
              typeEnv = Map.insert name tipe (typeEnv fragment),
              typeConstraint = name <? tipe /\ typeConstraint fragment
            }

      PData name patterns -> do
          (cvars, ctipe) <- freshDataScheme env name
          let (argTypes, resultType) = flattenFunction ctipe
              msg = "Constructor '" ++ name ++ "' expects " ++ show (length argTypes) ++ " arguments."
          if length patterns /= length argTypes then error msg else do
              fragment <- Monad.liftM joinFragments (Monad.zipWithM (constrain env) patterns argTypes)
              return $ fragment {
                typeConstraint = typeConstraint fragment /\ tipe === resultType,
                vars = cvars ++ vars fragment
              }

      PRecord fields -> do
          pairs <- mapM (\name -> (,) name <$> flexibleVar) fields
          let tenv = Map.fromList (map (second VarN) pairs)
          c <- exists $ \t -> return (tipe === record (Map.map (:[]) tenv) t)
          return $ Fragment {
              typeEnv        = tenv,
              vars           = map snd pairs,
              typeConstraint = c
          }

freshDataScheme :: Environment -> [Char] -> IO ([Variable], Type)
freshDataScheme env name =
  do let (vars, tipe) = Env.get env constructor name
     freshVars <- mapM (\_ -> flexibleVar) vars
     let assocList = zip vars freshVars
     return (freshVars, substitute assocList tipe)

substitute :: Eq a => [(a, a)] -> TermN a -> TermN a
substitute assocs tipe =
    case tipe of
      VarN x -> VarN (Maybe.fromMaybe x (List.lookup x assocs))
      TermN t -> TermN $
          case t of
            App1 a b -> App1 (substitute assocs a) (substitute assocs b)
            Fun1 a b -> Fun1 (substitute assocs a) (substitute assocs b)
            Var1 v -> Var1 (substitute assocs v)
            EmptyRecord1 -> EmptyRecord1
            Record1 fields extension ->
                Record1 (Map.map (map $ substitute assocs) fields)
                        (substitute assocs extension)

flattenFunction :: TermN t -> ([TermN t], TermN t)
flattenFunction = go []
    where go args tipe =
            case tipe of
              TermN (Fun1 a b) -> go (a:args) b
              _ -> (reverse args, tipe)
