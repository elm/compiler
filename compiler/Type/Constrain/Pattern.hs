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
          (kind, cvars, ctipe) <- freshDataScheme env name
          tvars <- mapM (\_ -> VarN <$> flexibleVar) [1..kind]
          let tipe' = foldr (==>) tipe tvars
              msg = concat [ "Constructor '", name, "' expects ", show kind
                           , " argument", if kind == 1 then "" else "s"
                           , " but was given ", show (length patterns), "." ]
          if length patterns /= kind then error msg else do
              fragment <- Monad.liftM joinFragments (Monad.zipWithM (constrain env) patterns tvars)
              return $ fragment {
                typeConstraint = typeConstraint fragment /\ tipe' === ctipe,
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
