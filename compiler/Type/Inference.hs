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
import qualified AST.Variable as Var
import Text.PrettyPrint
import qualified Type.State as TS
import qualified Type.ExtraChecks as Check
import Control.Arrow (first, second)
import Control.Monad.State (execStateT, forM)
import Control.Monad.Error (ErrorT, runErrorT, liftIO)

import System.IO.Unsafe  -- Possible to switch over to the ST monad instead of
                         -- the IO monad. I don't think that'd be worthwhile.


infer :: Interfaces -> CanonicalModule -> Either [Doc] (Map.Map String CanonicalType)
infer interfaces modul = unsafePerformIO $ do
  let moduleBody = body modul

  env <- Env.initialEnvironment (canonicalizeAdts interfaces modul)
  ctors <- forM (Map.keys (Env.constructor env)) $ \name ->
               do (_, vars, args, result) <- Env.freshDataScheme env name
                  return (name, (vars, foldr (T.==>) result args))

  attemptConstraint <- runErrorT $ do
    importedVars <- mapM (canonicalizeValues env) (Map.toList interfaces)

    let allTypes = concat (ctors : importedVars)
        vars = concatMap (fst . snd) allTypes
        header = Map.map snd (Map.fromList allTypes)
        environ = noneNoDocs . T.CLet [ T.Scheme vars [] (noneNoDocs T.CTrue) header ]

    fvar <- liftIO $ T.variable T.Flexible
    c <- TcExpr.constrain env (program moduleBody) (T.varN fvar)
    return (header, environ c)

  case attemptConstraint of
    Left err -> return $ Left err
    Right (header, constraint) -> do
      state <- execStateT (Solve.solve constraint) TS.initialState
      case TS.sErrors state of
        errors@(_:_) -> return (Left errors)
        [] -> case Check.portTypes (program moduleBody) of
                Left err -> return (Left err)
                Right () -> Check.mainType types
                    where
                      header' = Map.delete "::" header
                      types = Map.difference (TS.sSavedEnv state) header'

canonicalizeValues :: Env.Environment -> (String, Interface)
                   -> ErrorT [Doc] IO [(String, ([T.Variable], T.Type))]
canonicalizeValues env (moduleName, iface) =
    forM (Map.toList (iTypes iface)) $ \(name,tipe) -> do
      tipe' <- Env.instantiateType env tipe Map.empty
      return (moduleName ++ "." ++ name, tipe')

canonicalizeAdts :: Interfaces -> CanonicalModule -> [CanonicalAdt]
canonicalizeAdts interfaces modul =
    localAdts ++ importedAdts
  where
    localAdts :: [CanonicalAdt]
    localAdts = format (Module.getName modul, datatypes (body modul))

    importedAdts :: [CanonicalAdt]
    importedAdts = concatMap (format . second iAdts) (Map.toList interfaces)

    format :: (String, Module.ADTs) -> [CanonicalAdt]
    format (home, adts) =
        map canonical (Map.toList adts)
      where
        canonical :: (String, AdtInfo String) -> CanonicalAdt
        canonical (name, (tvars, ctors)) =
            ( toVar name
            , (tvars, map (first toVar) ctors)
            )

        toVar :: String -> Var.Canonical
        toVar = Var.Canonical (Var.Module home)
