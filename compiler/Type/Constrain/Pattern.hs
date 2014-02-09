{-# OPTIONS_GHC -W #-}
{-# LANGUAGE FlexibleInstances #-}
module Type.Constrain.Pattern where

import Control.Arrow (second)
import Control.Applicative ((<$>))
import qualified Control.Monad as Monad
import Control.Monad.Error
import qualified Data.Map as Map
import qualified Text.PrettyPrint as PP

import qualified SourceSyntax.Annotation as A
import qualified SourceSyntax.Pattern as P
import SourceSyntax.PrettyPrint (pretty)
import Type.Type
import Type.Fragment
import Type.Environment as Env
import qualified Type.Constrain.Literal as Literal


constrain :: Environment -> P.Pattern -> Type -> ErrorT (A.Region -> PP.Doc) IO Fragment
constrain env pattern tipe =
    let region = A.None (pretty pattern)
        t1 === t2 = A.A region (CEqual t1 t2)
        x <? t = A.A region (CInstance x t)
    in
    case pattern of
      P.Anything -> return emptyFragment

      P.Literal lit -> do
          c <- liftIO $ Literal.constrain env region lit tipe
          return $ emptyFragment { typeConstraint = c }

      P.Var name -> do
          v <- liftIO $ var Flexible
          return $ Fragment {
              typeEnv    = Map.singleton name (VarN v),
              vars       = [v],
              typeConstraint = VarN v === tipe
          }

      P.Alias name p -> do
          fragment <- constrain env p tipe
          return $ fragment {
              typeEnv = Map.insert name tipe (typeEnv fragment),
              typeConstraint = name <? tipe /\ typeConstraint fragment
            }

      P.Data name patterns -> do
          (kind, cvars, args, result) <- liftIO $ freshDataScheme env name
          let msg = concat [ "Constructor '", name, "' expects ", show kind
                           , " argument", if kind == 1 then "" else "s"
                           , " but was given ", show (length patterns), "." ]
              err span = PP.vcat [ PP.text $ "Type error " ++ show span
                                 , PP.text msg ]
          case length patterns == kind of
            False -> throwError err
            True -> do
              fragment <- Monad.liftM joinFragments (Monad.zipWithM (constrain env) patterns args)
              return $ fragment {
                typeConstraint = typeConstraint fragment /\ tipe === result,
                vars = cvars ++ vars fragment
              }

      P.Record fields -> do
          pairs <- liftIO $ mapM (\name -> (,) name <$> var Flexible) fields
          let tenv = Map.fromList (map (second VarN) pairs)
          c <- exists $ \t -> return (tipe === record (Map.map (:[]) tenv) t)
          return $ Fragment {
              typeEnv        = tenv,
              vars           = map snd pairs,
              typeConstraint = c
          }

instance Error (A.Region -> PP.Doc) where
  noMsg _ = PP.empty
  strMsg str span =
      PP.vcat [ PP.text $ "Type error " ++ show span
              , PP.text str ]
