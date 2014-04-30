{-# OPTIONS_GHC -W #-}
module Type.Inference where

import qualified Data.Map as Map

import qualified Type.Type as T
import qualified Type.Environment as Env
import qualified Type.Constrain.Expression as TcExpr
import qualified Type.Solve as Solve

import AST.Module as Module
import AST.Annotation (noneNoDocs)
import AST.Type (CanonicalType)
import Text.PrettyPrint
import qualified Type.State as TS
import qualified Type.ExtraChecks as Check
import Control.Monad.State (execStateT, forM)
import Control.Monad.Error (runErrorT, liftIO)
import qualified Type.Alias as Alias

import System.IO.Unsafe  -- Possible to switch over to the ST monad instead of
                         -- the IO monad. I don't think that'd be worthwhile.


infer :: Interfaces -> CanonicalModule -> Either [Doc] (Map.Map String CanonicalType)
infer interfaces modul = unsafePerformIO $ do
  let moduleBody = body modul
      adts = Map.unions (datatypes moduleBody : map iAdts (Map.elems interfaces))
  env <- Env.initialEnvironment adts
  ctors <- forM (Map.keys (Env.constructor env)) $ \name ->
               do (_, vars, args, result) <- Env.freshDataScheme env name
                  return (name, (vars, foldr (T.==>) result args))

  attemptConstraint <- runErrorT $ do
    importedVars <-
        forM (concatMap (Map.toList . iTypes) $ Map.elems interfaces) $ \(name,tipe) ->
            (,) name `fmap` Env.instantiateType env tipe Map.empty

    let allTypes = ctors ++ importedVars
        vars = concatMap (fst . snd) allTypes
        header = Map.map snd (Map.fromList allTypes)
        environ = noneNoDocs . T.CLet [ T.Scheme vars [] (noneNoDocs T.CTrue) header ]

    fvar <- liftIO $ T.var T.Flexible
    c <- TcExpr.constrain env (program moduleBody) (T.VarN fvar)
    return (header, environ c)

  case attemptConstraint of
    Left err -> return $ Left err
    Right (header, constraint) -> do
      state <- execStateT (Solve.solve constraint) TS.initialState
      let rules = Alias.rules interfaces (aliases moduleBody) (imports modul)
      case TS.sErrors state of
        errors@(_:_) -> Left `fmap` sequence (map ($ rules) (reverse errors))
        [] -> case Check.portTypes rules (program moduleBody) of
                Right () -> Check.mainType rules (Map.difference (TS.sSavedEnv state) header)
                Left err -> return (Left err)
